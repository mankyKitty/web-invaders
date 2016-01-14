module Main where

import Prelude

import Control.Apply

import Data.Maybe hiding (fromMaybe)

import Graphics.Canvas

import DOM (DOM())

import DOM.HTML (window)
import DOM.HTML.Window (document)
import DOM.HTML.Types (htmlDocumentToDocument,htmlDocumentToNonElementParentNode)
import DOM.Node.Types (NonElementParentNode(),Element(),ElementId(ElementId))

import DOM.Node.NonElementParentNode (getElementById)

import Data.Lens ((^.))
import Data.Nullable (toMaybe)

import Control.Timer (Timer())
import Control.Monad

import Control.Monad.Eff
import Control.Monad.Eff.Exception (EXCEPTION(),catchException, throw, message)
import Control.Monad.Eff.Console (CONSOLE(), error, log)

import Signal (foldp, runSignal, sampleOn, Signal())
import Signal.DOM (animationFrame, keyPressed)
import Signal.Time (Time())

import Sprite (Sprite(),DimensionPair(),mkSprite,getFrame,_Element)
import Utils (drawImageFromElement,drawSpriteFrame)

upKeyCode :: Int
upKeyCode = 38

downKeyCode :: Int
downKeyCode = 40

leftKeyCode :: Int
leftKeyCode = 37

rightKeyCode :: Int
rightKeyCode = 39

type KeyInput =
  { left :: Boolean
  , right :: Boolean
  , up :: Boolean
  , down :: Boolean
  }

type GameState = 
  { ctx :: Context2D
  , box :: Rectangle
  , screen :: Rectangle
  , images :: {
    ship :: Element,
    boom :: { sprite :: Sprite
            , frame :: Int
          }
        }
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
canvasRect = { x: 0.0, y: 0.0, w: 600.0, h: 600.0 }

startState 
  :: Context2D 
  -> Element
  -> Sprite
  -> EffGame () GameState
startState ctx ship boom =
  pure { ctx: ctx
       , box: { x: 250.0 , y: 250.0 , w: 100.0 , h: 100.0 } 
       , screen: canvasRect
       , images: { ship: ship , boom: { sprite: boom, frame: 0 } }
       }

requestContext
  :: String
  -> EffGame () Context2D
requestContext elemId = 
  getCanvasElementById elemId >>= maybe err getContext2D
  where
    err = throw $ "Unable to find Canvas element with #:" <> elemId

moveDist :: Number
moveDist = 10.0 

moveBox 
  :: KeyInput 
  -> Rectangle 
  -> Rectangle
moveBox { left: l, right: r, up: u, down: d } rect = mv l r u d
  where mv true false true false = rect { x = rect.x - moveDist, y = rect.y - moveDist }
        mv true false false true = rect { x = rect.x - moveDist, y = rect.y + moveDist }
        mv false true true false = rect { x = rect.x + moveDist, y = rect.y - moveDist }
        mv false true false true = rect { x = rect.x + moveDist, y = rect.y + moveDist }
        mv true false false false = rect { x = rect.x - moveDist }
        mv false true false false = rect { x = rect.x + moveDist }
        mv false false true false = rect { y = rect.y - moveDist }
        mv false false false true = rect { y = rect.y + moveDist }
        mv _ _ _ _ = rect

mkKeyInput
  :: Boolean
  -> Boolean
  -> Boolean
  -> Boolean
  -> KeyInput
mkKeyInput = 
  { left: _, right: _, up: _, down: _ }

upState
  :: KeyInput
  -> EffGame () GameState
  -> EffGame () GameState
upState input gs = do
  game <- gs
  pure <<< upSpriteFrame $ game { box = moveBox input game.box }
  where
    upSpriteFrame :: GameState -> GameState
    upSpriteFrame g = 
      if g.images.boom.frame == 0 || g.images.boom.frame < 12
        then g { images = g.images { boom = g.images.boom { frame = g.images.boom.frame + 1 } } }
        else g { images = g.images { boom = g.images.boom { frame = 0 } } }

render :: EffGame () GameState -> EffGame () Unit
render g = do
  gameSt <- g
  spriteFrame <- sFrame gameSt
  clearRect gameSt.ctx gameSt.screen
  setFillStyle "#0000FF" gameSt.ctx
  drawImageFromElement gameSt.ctx gameSt.images.ship gameSt.box.x gameSt.box.y
  drawSpriteFrame gameSt.ctx (gameSt.images.boom.sprite ^. _Element) 230.0 230.0 spriteFrame
  pure unit
  where
    sFrame g = maybe (throw "Invalid sprite frame!") pure $
      getFrame g.images.boom.sprite g.images.boom.frame

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
  boomsImg <- (\b -> mkSprite b { h: 40.0, w: 39.0 } 13) <$> loadImg "boomsImage" docNEPN
  shipImg <- loadImg "shipImage" docNEPN
  -- Create input signals
  leftIn  <- keyPressed leftKeyCode
  rightIn <- keyPressed rightKeyCode
  upIn    <- keyPressed upKeyCode
  downIn  <- keyPressed downKeyCode
  -- Build input producer
  let inps = mkKeyInput <$> leftIn <*> rightIn <*> upIn <*> downIn
  -- Build game loop
  let game = foldp upState (startState c shipImg boomsImg) (sampleOn frames inps)
  -- run game loop using input signals
  runSignal (render <$> game)
