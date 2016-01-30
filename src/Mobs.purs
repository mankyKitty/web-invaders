module Mobs where

import Prelude

import Data.Int (toNumber)

import Math as Math
import Data.Array (replicate)

import Data.Lens (LensP(),(%~),(^.),(.~),to,lens)

import Graphics.Canvas (Rectangle())
import Sprite (CoordinatePair(),_X,_Y,ranger)

data Jinking
  = Left
  | Right

data Mob = Mob Jinking CoordinatePair

mobHeading :: LensP Mob Jinking
mobHeading = lens (\(Mob h _) -> h) (\(Mob _ c) h -> Mob h c)

mobCoord :: LensP Mob CoordinatePair
mobCoord = lens (\(Mob _ c) -> c) (\(Mob h _) c -> Mob h c)

mobX :: LensP Mob Number
mobX = mobCoord <<< _X

mobY :: LensP Mob Number
mobY = mobCoord <<< _Y

addMobs :: Int -> Number -> CoordinatePair -> Array Mob
addMobs n buffer start = do
  mob <- replicate n start
  mobN <- ranger 1.0 $ toNumber n
  pure <<< Mob Right $ mob { y = mob.y + (mobN * buffer), x = mob.x + (mobN * buffer) }

moveMobs :: Number -> Rectangle -> Array Mob -> Array Mob
moveMobs vel box = map (wibble <<< setJink)
  where
    wibble m = m # mobY %~ (+ (-(Math.cos (m ^. mobX <<< to (/ 100.0)) ) * 5.0))

    setJink m@(Mob Right c) =
      if (c ^. _X) + vel >= box.w
      then m # (mobHeading .~ Left) # (mobX %~ (\x -> x - vel))
      else m # mobX %~ (+ vel)

    setJink m@(Mob Left c) =
      if (c ^. _X) - vel <= 0.0
      then m # (mobHeading .~ Right) # (mobX %~ (+ vel))
      else m # mobX %~ (\x -> x - vel)
