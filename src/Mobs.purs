module Mobs where

import Prelude

import Math as Math
import Data.Array (replicate)

import Data.Lens ((%~),(^.))

import Sprite (CoordinatePair(),_X,_Y)
import Utils ((&))

addMobs :: Int -> CoordinatePair -> Array CoordinatePair
addMobs = replicate

moveMobs :: Number -> Array CoordinatePair -> Array CoordinatePair
moveMobs vel = map moveEeet
  where
    moveEeet m = m
      & (_X %~ (+ vel))
      & (_Y %~ (+ (-(Math.cos (m ^. _X / 100.0) ) * 5.0)))
