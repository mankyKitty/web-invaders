module Utils 
  ( drawImageFromElement
  ) where

import Control.Monad.Eff (Eff())

import Graphics.Canvas
import DOM.Node.Types (Element())

foreign import drawImageFromElement
  :: forall eff. Context2D 
  -> Element 
  -> Number 
  -> Number 
  -> Eff (canvas :: Canvas | eff) Context2D

