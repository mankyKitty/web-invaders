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
  , playerScore :: Int
  , playerLives :: Int
  , mobs :: Array Mob
  , mobCount :: Int
  , mobWub :: (Mob -> Mob)
  , mobsImg :: Element
  , mobBulletImg :: Element
  , mobBullets :: Array CoordinatePair
  , lastFired :: Time
  , lastMobFired :: Time
  , gameFinished :: Boolean
  }

gameFinished :: forall r. LensP { gameFinished :: Boolean | r } Boolean
gameFinished = lens _.gameFinished (_ { gameFinished = _ })

mobCount :: forall r. LensP { mobCount :: Int | r } Int
mobCount = lens _.mobCount (_ { mobCount = _ })

playerScore :: forall r. LensP { playerScore :: Int | r } Int
playerScore = lens _.playerScore (_ { playerScore = _ })

playerLives :: forall r. LensP { playerLives :: Int | r } Int
playerLives = lens _.playerLives (_ { playerLives = _ })

mobWub :: forall r. LensP { mobWub :: (Mob -> Mob) | r } (Mob -> Mob)
mobWub = lens _.mobWub (_ { mobWub = _ })

mobBulletImg :: forall r. LensP { mobBulletImg :: Element | r } Element
mobBulletImg = lens _.mobBulletImg (_ { mobBulletImg = _})

mobBullets :: forall r. LensP { mobBullets :: Array CoordinatePair | r } (Array CoordinatePair)
mobBullets = lens _.mobBullets (_ { mobBullets = _})

mBullets :: TraversalP GameState CoordinatePair
mBullets = mobBullets <<< traversed

mobsImg :: forall r. LensP { mobsImg :: Element | r } Element
mobsImg = lens _.mobsImg (_ { mobsImg = _ })

mobs :: forall r. LensP { mobs :: Array Mob | r } (Array Mob)
mobs = lens _.mobs (_ { mobs = _ })

lastFired :: forall r. LensP { lastFired :: Time | r } Time
lastFired = lens _.lastFired (_ { lastFired = _ })

lastMobFired :: forall r. LensP { lastMobFired :: Time | r } Time
lastMobFired = lens _.lastMobFired (_ { lastMobFired = _ })

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
