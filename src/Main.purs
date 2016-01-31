module Main where

import Prelude

import Data.Foldable (traverse_,any,foldr)

import Data.Tuple
import Data.Maybe hiding (fromMaybe)
import Data.Array (length,cons,filter,(:), null)
import Graphics.Canvas

import DOM (DOM())

import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.Node.Types (NonElementParentNode(),Element(),ElementId(ElementId))

import DOM.Node.NonElementParentNode (getElementById)

import Data.Lens ((%~), (^.), (.~), (++~), (+~),to)
import Data.Nullable (toMaybe)

import Control.Timer (Timer())

import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION(), throw)
import Control.Monad.Eff.Console (CONSOLE())

import Signal (Signal(), runSignal, sampleOn)
import Signal.Time (Time())
import Signal.DOM (animationFrame)

import Control.Monad.ST

import Sprite (Sprite(),CoordinatePair(),DimensionPair(),_Y)
import Utils (drawImageFromElement,rectInter)
import Input (KeyInput(),readInput)

import Mobs
import GameState

type BoomSprite =
  { sprite :: Sprite
  , frame :: Int
  }

type EffGame eff a =
  Eff ( err :: EXCEPTION
      , canvas :: Canvas
      , timer :: Timer
      , dom :: DOM
      , console :: CONSOLE
      | eff
      ) a

playerMoveDist :: Number
playerMoveDist = 6.0

playerRect :: DimensionPair
playerRect = { w: 43.0, h: 43.0 }

mobRect :: DimensionPair
mobRect = { w: 24.0, h: 35.0 }

mBulletRect :: DimensionPair
mBulletRect = { w: 10.0, h: 12.0 }

bulletRect :: DimensionPair
bulletRect = { w: 11.0, h: 16.0 }

bulletSpeed :: Number
bulletSpeed = 10.0

canvasRect :: Rectangle
canvasRect = { x: 0.0, y: 0.0, w: 320.0, h: 480.0 }

playerStartLoc :: CoordinatePair
playerStartLoc = { y: 440.0, x: 170.0 }
startState :: Context2D -> Element -> Element -> Element -> Element -> GameState
startState ctx ship bImg mImg mbImg =
  { ctx: ctx
  , screen: canvasRect
  , playerLoc: playerStartLoc
  , playerImg: ship
  , playerBulletImg: bImg
  , playerBullets: []
  , playerScore: 0
  , playerLives: 0
  , mobBullets: []
  , mobCount: initialMobCount
  , mobs: addMobs initialMobCount 30.0 $ { x: 10.0, y: 10.0 }
  , mobWub: wibble
  , mobBulletImg: mbImg
  , mobsImg: mImg
  , lastFired: 0.0
  , lastMobFired: 1500.00
  , gameFinished: false
  }
  where
    initialMobCount = 7

requestContext :: String -> EffGame () Context2D
requestContext elemId =
  getCanvasElementById elemId >>= maybe err getContext2D
  where
    err = throw $ "Unable to find Canvas element with #:" <> elemId


moveCoordByInput :: Number -> KeyInput -> Rectangle -> CoordinatePair -> CoordinatePair
moveCoordByInput dist { left: l, right: r, up: u, down: d } box c = mv l r u d
  where mv true false true false = c { x = xLDist, y = yUDist }
        mv true false false true = c { x = xLDist, y = yDDist }
        mv false true true false = c { x = xRDist, y = yUDist }
        mv false true false true = c { x = xRDist, y = yDDist }
        mv true false false false = c { x = xLDist }
        mv false true false false = c { x = xRDist }
        mv false false true false = c { y = yUDist }
        mv false false false true = c { y = yDDist }
        mv _ _ _ _ = c

        atMin dir p = if dir && p - dist <= 0.0 then p else p - dist
        atMax dir m p = if dir && p + dist >= m then p else p + dist

        yUDist = atMin u c.y
        yDDist = atMax d box.h c.y
        xLDist = atMin l c.x
        xRDist = atMax r box.w c.x

addPShot :: KeyInput -> (GameState -> GameState)
addPShot inp g =
  if triggerPressed && shotGap then addNewShot g else g
  where
    shotGap = (inp.frame) - (g.lastFired) >= 100.0

    triggerPressed = inp.space

    addNewShot gP = gP
      # (lastFired .~ (inp.frame))
      # (playerBullets %~ (cons <<< spawnBulletAt $ gP.playerLoc))

    -- Ugh...
    spawnBulletAt p = { x: p.x + 14.0, y: p.y - 15.0 }

addMobShots :: KeyInput -> (GameState -> GameState)
addMobShots i g =
  if mobsShoot g.lastMobFired i.frame
  then addShots g
  else g
  where
    addShots gP = gP
      # (lastMobFired .~ i.frame)
      # (mobBullets ++~ createMobBullets gP.mobs)

createMobBullets :: Array Mob -> Array CoordinatePair
createMobBullets = foldr (\(Mob _ c) acc -> mkBullet c : acc) []
  where
    mkBullet c = { x: c.x, y: c.y }

-- This is getting rather silly...
upState :: KeyInput -> GameState -> GameState
upState input =
  mobsEscaped <<<
  playerShot <<<
  respawnMobs <<<
  addMobShots input <<<
  addPShot input <<<
  pruneBullets <<<
  moveEnemyWave <<<
  mobShot <<<
  movePBullets <<<
  moveMBullets <<<
  movePlayer
  where
    mobsEscaped g = g # gameFinished .~ (any (\m -> m ^. mobY <<< to (> g.screen.h)) g.mobs)

    playerShot g =
      let pHit = (> 0) <<< length $ filter (\b -> rectInter b bulletRect g.playerLoc playerRect) g.mobBullets
      in case Tuple pHit g.playerLives of
        (Tuple false _) -> g
        --(Tuple true  0) -> g # gameFinished .~ true
        (Tuple true  n) -> g # playerLives +~ 1 # playerLoc .~ playerStartLoc

    respawnMobs g = if not $ null g.mobs then g
      else g # mobWub .~ (if mod input.frame 15.0 == 0.0 then wobble else wibble)
             # mobs .~ addMobs g.mobCount 20.0 { x: 5.0, y: 5.0 }

    mobShot g =
      let notShot = filter (not $ isShot g) g.mobs
          score = (* g.mobCount) <<< (* 100) <<< length $ filter (isShot g) g.mobs
      in g # mobs .~ notShot
           # playerScore +~ score

    isShot g m = any (\bP -> rectInter bP bulletRect (m ^. mobCoord) mobRect) g.playerBullets

    moveEnemyWave g = g # mobs %~ moveMobs 5.0 (g.mobWub) g.screen

    movePlayer g = g # playerLoc %~ moveCoordByInput playerMoveDist input g.screen

    movePBullets = pBullets %~ (_Y %~ (\y -> y - bulletSpeed))

    moveMBullets = mBullets %~ (_Y %~ (+ bulletSpeed))

    pruneBullets g = g
      # playerBullets %~ bulletGone
      # mobBullets %~ bulletGone

    bulletGone = filter (\p -> p.y >= 0.1)

  -- where
  --   upSpriteFrame :: GameState -> GameState
  --   upSpriteFrame g =
  --     if g.images.boom.frame == 0 || g.images.boom.frame < 12
  --       then g { images = g.images { boom = g.images.boom { frame = g.images.boom.frame + 1 } } }
  --       else g { images = g.images { boom = g.images.boom { frame = 0 } } }

drawElemAt
  :: forall e. Context2D
  -> Element
  -> CoordinatePair
  -> Eff (canvas :: Canvas | e) Unit
drawElemAt c e cp = void $ drawImageFromElement c e cp.x cp.y

render :: forall h. GameState -> EffGame (st :: ST h) Unit
render gameSt = do
  let ctx = gameSt ^. context2d
      dMany_ img =  traverse_ (drawElemAt ctx img)

  clearRect ctx gameSt.screen
  setFillStyle "#000000" ctx
  strokeText ctx ("Score: " <> show gameSt.playerScore) 20.0 40.0
  strokeText ctx ("Died: " <> show gameSt.playerLives) 20.0 70.0
  -- Draw the player
  drawElemAt ctx gameSt.playerImg gameSt.playerLoc
  -- Draw the players bullets
  dMany_ gameSt.playerBulletImg gameSt.playerBullets
  dMany_ gameSt.mobBulletImg gameSt.mobBullets
  -- Draw the monsters
  traverse_ (\m -> drawElemAt ctx gameSt.mobsImg (m ^. mobCoord)) $ gameSt ^. mobs

getDocumentNode :: forall eff. Eff (dom :: DOM | eff) NonElementParentNode
getDocumentNode = htmlDocumentToNonElementParentNode <$> (window >>= document)

getElemById
  :: forall eff. String
  -> NonElementParentNode
  -> Eff (dom :: DOM | eff) (Maybe Element)
getElemById str ne = toMaybe <$> getElementById (ElementId str) ne

loadImg
  :: forall eff. String
  -> NonElementParentNode
  -> Eff (dom :: DOM, err :: EXCEPTION | eff) Element
loadImg s dom = getElemById s dom >>= maybe (throw $ "Couldn't load: " <> s) pure

renderGameOver :: forall h. GameState -> EffGame (st :: ST h) Unit
renderGameOver g = do
  strokeText (g ^. context2d) "GAME OVER!" 20.0 240.0
  strokeText (g ^. context2d) "Refresh to play." 20.0 280.0
  pure unit

main :: EffGame () Unit
main = do
  frames <- animationFrame
  docNEPN <- getDocumentNode
  ctx <- requestContext "canvas"
  setFont "36px serif" ctx
  --ctx.strokeText("Hello world", 10, 50);
  -- Image
  st <- startState
        <$> pure ctx
        <*> loadImg "shipImage-normal" docNEPN
        <*> loadImg "player-bullet" docNEPN
        <*> loadImg "mob-one-img" docNEPN
        <*> loadImg "mob-bullet-img" docNEPN
  -- Create input signal
  inps <- readInput
  -- Build game loop
  runST (runGame st inps frames)
  where
    mkGoNow :: forall h. STRef h GameState -> KeyInput -> EffGame (st :: ST h) Unit
    mkGoNow g k = do
      gs <- readSTRef g
      if (gs.gameFinished)
        then renderGameOver gs
        else (modifySTRef g (upState k) >>= render)

    runGame :: forall h. GameState -> Signal KeyInput -> Signal Time -> EffGame (st :: ST h) Unit
    runGame st inp fs = do
      sRef <- newSTRef st
      let game = sampleOn fs inp
      runSignal (mkGoNow sRef <$> game)
