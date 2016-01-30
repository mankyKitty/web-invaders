module Main where

import Prelude

import Data.Foldable (traverse_)

import Data.Maybe hiding (fromMaybe)
import Data.Array (cons,filter)
import Graphics.Canvas

import DOM (DOM())

import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.Node.Types (NonElementParentNode(),Element(),ElementId(ElementId))

import DOM.Node.NonElementParentNode (getElementById)

import Data.Lens ((%~), (^.), (.~))
import Data.Nullable (toMaybe)

import Control.Timer (Timer())

import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION(), throw)
import Control.Monad.Eff.Console (CONSOLE())

import Signal (foldp, runSignal, sampleOn)
import Signal.DOM (animationFrame)

import Sprite (Sprite(),CoordinatePair(),_X,_Y)
import Utils (drawImageFromElement)
import Input (KeyInput(),readInput,space,frame)

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

canvasRect :: Rectangle
canvasRect = { x: 0.0, y: 0.0, w: 320.0, h: 480.0 }

startState :: Context2D -> Element -> Element -> Element -> EffGame () GameState
startState ctx ship bImg mImg =
  pure { ctx: ctx
       , screen: canvasRect
       , playerLoc: { y: 440.0, x: 170.0 }
       , playerImg: ship
       , playerBulletImg: bImg
       , playerBullets: []
       , mobs: addMobs 7 30.0 $ { x: 10.0, y: 10.0 }
       , mobsImg: mImg
       , lastFired: 0.0
       }

requestContext :: String -> EffGame () Context2D
requestContext elemId =
  getCanvasElementById elemId >>= maybe err getContext2D
  where
    err = throw $ "Unable to find Canvas element with #:" <> elemId

playerMoveDist :: Number
playerMoveDist = 6.0

bulletSpeed :: Number
bulletSpeed = 10.0

moveCoordByInput :: Number -> KeyInput -> CoordinatePair -> CoordinatePair
moveCoordByInput dist { left: l, right: r, up: u, down: d } c = mv l r u d
  where mv true false true false = c { x = c.x - dist, y = c.y - dist }
        mv true false false true = c { x = c.x - dist, y = c.y + dist }
        mv false true true false = c { x = c.x + dist, y = c.y - dist }
        mv false true false true = c { x = c.x + dist, y = c.y + dist }
        mv true false false false = c { x = c.x - dist }
        mv false true false false = c { x = c.x + dist }
        mv false false true false = c { y = c.y - dist }
        mv false false false true = c { y = c.y + dist }
        mv _ _ _ _ = c

addPShot :: KeyInput -> (GameState -> GameState)
addPShot inp g = if triggerPressed && shotGap
                 then addNewShot g
                 else g
  where
    shotGap = (inp ^. frame) - (g ^. lastFired) >= 100.0

    triggerPressed = inp ^. space

    addNewShot gP = gP
      # (lastFired .~ (inp ^. frame))
      # (playerBullets %~ (cons <<< spawnBulletAt $ gP ^. playerLoc))

    -- Ugh...
    spawnBulletAt p = { x: p.x + 14.0, y: p.y - 15.0 }

upState
  :: KeyInput
  -> EffGame () GameState
  -> EffGame () GameState
upState input gs =
  addPShot input
  <<< pruneBullets
  <<< moveEnemyWave
  <<< moveBullets
  <<< movePlayer
  <$> gs
  where
    moveEnemyWave g = g # mobs %~ moveMobs 5.0 g.screen
    movePlayer = playerLoc %~ moveCoordByInput playerMoveDist input
    moveBullets = pBullets %~ (_Y %~ (\y -> y - bulletSpeed))
    pruneBullets = playerBullets %~ filter (\p -> p.y >= 0.1)

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
drawElemAt c e cp = void $ drawImageFromElement c e (cp ^. _X) (cp ^. _Y)

render :: EffGame () GameState -> EffGame () Unit
render g = do
  gameSt <- g
  let ctx = gameSt ^. context2d
      dMany_ img =  traverse_ (drawElemAt ctx img)

  clearRect ctx gameSt.screen
  setFillStyle "#0000FF" ctx
  -- Draw the player
  drawElemAt ctx (gameSt ^. playerImg) (gameSt ^. playerLoc)
  -- Draw the players bullets
  dMany_ (gameSt ^. playerBulletImg) $ gameSt ^. playerBullets
  -- Draw the monsters
  traverse_ (\m -> drawElemAt ctx (gameSt ^. mobsImg) (m ^. mobCoord)) $ gameSt ^. mobs

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

main :: EffGame () Unit
main = do
  -- Init
  c <- requestContext "canvas"
  frames <- animationFrame
  docNEPN <- getDocumentNode
  -- Image loading
  initState <- startState
               <$> requestContext "canvas"
               <*> loadImg "shipImage-normal" docNEPN
               <*> loadImg "player-bullet" docNEPN
               <*> loadImg "mob-one-img" docNEPN

  -- Create input signal
  inps <- readInput
  -- Build game loop
  let game = foldp upState initState (sampleOn frames inps)
  -- run game loop using input signals
  runSignal (render <$> game)
