module Sprite where

import Prelude

import Data.Int (toNumber)

import Data.Lens
import Data.Lens.At
import Data.Lens.Index

import Data.Unfoldable (unfoldr)
import Data.Tuple
import Data.Maybe (Maybe(Just,Nothing),maybe,fromMaybe)

import Data.Array hiding ((..))
import Data.NonEmpty (NonEmpty(NonEmpty),fromNonEmpty)

import DOM.Node.Types (Element())

-- I can't use the types from Signal (DimensionPair,CoordinatePair)
-- as they use Ints and I need Number. :<

type DimensionPair = { w :: Number, h :: Number }
type CoordinatePair = { x :: Number, y :: Number }

newtype SpriteFrames = SpriteFrames (NonEmpty Array CoordinatePair)

data Sprite = Sprite
  { dimensions :: DimensionPair
  , frames :: SpriteFrames
  , element :: Element
  , frame :: Int
  }

_Width :: LensP DimensionPair Number
_Width = lens _.w (_ { w = _ })

_Height :: LensP DimensionPair Number
_Height = lens _.h (_ { h = _ })

_X :: LensP CoordinatePair Number
_X = lens _.x (_ { x = _ })

_Y :: LensP CoordinatePair Number
_Y = lens _.y (_ { y = _ })

_Element :: LensP Sprite Element
_Element = lens (\(Sprite s) -> s.element) (\(Sprite s) e -> Sprite $ s { element = e })

getFrame :: Sprite -> Int -> Maybe (Tuple CoordinatePair DimensionPair)
getFrame (Sprite { dimensions = d, frames = SpriteFrames fs }) n =
  (\c -> Tuple c d) <$> (flip index n $ fromNonEmpty (:) fs)

ranger :: Number -> Number -> Array Number
ranger k n = unfoldr numnum k
  where
    numnum kP = 
      if kP < n then Just (Tuple kP $ kP + 1.0) else Nothing

mkSprite :: Element -> DimensionPair -> Int -> Sprite
mkSprite e dim framesN = Sprite {
  dimensions: dim,
  element: e,
  frames: frames }
  where
    frames = SpriteFrames <<< NonEmpty { x: 0.0, y: 0.0 } <<<
             map (\x -> { y: 0.0, x: (x * dim.w) }) <<< ranger 1.0 $ toNumber framesN




