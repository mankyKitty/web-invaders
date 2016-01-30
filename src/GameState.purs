module GameState where

import Prelude

import Graphics.Canvas (Context2D(),Rectangle())
import DOM.Node.Types (Element())
import Signal.Time (Time())

import Sprite (CoordinatePair())
import Mobs (Mob())

import Data.Lens (LensP(),TraversalP(),lens,traversed)

type GameState =
  { ctx :: Context2D
  , screen :: Rectangle
  , playerLoc :: CoordinatePair
  , playerImg :: Element
  , playerBulletImg :: Element
  , playerBullets :: Array CoordinatePair
  , mobs :: Array Mob
  , mobsImg :: Element
  , lastFired :: Time
  }

mobsImg :: forall r. LensP { mobsImg :: Element | r } Element
mobsImg = lens _.mobsImg (_ { mobsImg = _ })

mobs :: forall r. LensP { mobs :: Array Mob | r } (Array Mob)
mobs = lens _.mobs (_ { mobs = _ })

lastFired :: forall r. LensP { lastFired :: Time | r } Time
lastFired = lens _.lastFired (_ { lastFired = _ })

context2d :: forall r. LensP { ctx :: Context2D | r } Context2D
context2d = lens _.ctx (_ { ctx = _ })

screen :: forall r. LensP { screen :: Rectangle | r } Rectangle
screen = lens _.screen (_ { screen = _ })

playerLoc :: forall r. LensP { playerLoc :: CoordinatePair | r } CoordinatePair
playerLoc = lens _.playerLoc (_ { playerLoc = _ })

playerImg :: forall r. LensP { playerImg :: Element | r } Element
playerImg = lens _.playerImg (_ { playerImg = _ })

playerBulletImg :: forall r. LensP { playerBulletImg :: Element | r } Element
playerBulletImg = lens _.playerBulletImg (_ { playerBulletImg = _ })

playerBullets :: forall r. LensP { playerBullets :: Array CoordinatePair | r } (Array CoordinatePair)
playerBullets = lens _.playerBullets (_ { playerBullets = _ })

pBullets :: TraversalP GameState CoordinatePair
pBullets = playerBullets <<< traversed
