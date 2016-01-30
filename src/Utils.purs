module Utils 
  ( drawImageFromElement
  , drawImageFromElementScale
  , drawSpriteFrame
  ) where

import Data.Lens ((^.))
import Data.Tuple
import Control.Monad.Eff (Eff())

import Graphics.Canvas
import DOM.Node.Types (Element())

import Sprite (CoordinatePair(),DimensionPair(),_Width,_Height,_X,_Y)

drawSpriteFrame
  :: forall eff. Context2D
  -> Element
  -> Number -- dest x
  -> Number -- dest y
  -> Tuple CoordinatePair DimensionPair
  -> Eff (canvas :: Canvas | eff) Context2D
drawSpriteFrame ctx e dx dy (Tuple coord dim) =
  drawImageFinalForm ctx e coX coY dW dH dx dy dW dH 
  where
    dW = dim ^. _Width
    dH = dim ^. _Height
    coX = coord ^. _X
    coY = coord ^. _Y

foreign import drawImageFinalForm
  :: forall eff. Context2D
  -> Element
  -> Number -- dest x
  -> Number -- dest y
  -> Number -- dest Width
  -> Number -- dest Height
  -> Number -- Source x
  -> Number -- Source y
  -> Number -- Source Width
  -> Number -- Source Height
  -> Eff (canvas :: Canvas | eff) Context2D

foreign import drawImageFromElement
  :: forall eff. Context2D
  -> Element
  -> Number
  -> Number
  -> Eff (canvas :: Canvas | eff) Context2D

foreign import drawImageFromElementScale
  :: forall eff. Context2D
  -> Element
  -> Number
  -> Number
  -> Number
  -> Number
  -> Eff (canvas :: Canvas | eff) Context2D
