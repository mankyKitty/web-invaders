module Input where

import Prelude

import Signal (Signal()) -- ,(~>),sampleOn)
import Signal.DOM (keyPressed, animationFrame)
import Signal.Time (Time())

import Control.Timer (Timer())

import Data.Lens (LensP(),lens)
import Control.Monad.Eff (Eff())
import DOM (DOM())

upKeyCode :: Int
upKeyCode = 38

downKeyCode :: Int
downKeyCode = 40

leftKeyCode :: Int
leftKeyCode = 37

rightKeyCode :: Int
rightKeyCode = 39

spaceKeyCode :: Int
spaceKeyCode = 32

type KeyInput =
  { left  :: Boolean
  , right :: Boolean
  , up    :: Boolean
  , down  :: Boolean
  , space :: Boolean
  , frame :: Time
  }

limitShooting :: forall e. Eff (dom :: DOM | e) (Signal Boolean)
limitShooting = keyPressed spaceKeyCode

left :: forall r. LensP { left :: Boolean | r } Boolean
left = lens _.left (_ { left = _ })

right :: forall r. LensP { right :: Boolean | r } Boolean
right = lens _.right (_ { right = _ })

up :: forall r. LensP { up :: Boolean | r } Boolean
up = lens _.up (_ { up = _ })

down :: forall r. LensP { down :: Boolean | r } Boolean
down = lens _.down (_ { down = _ })

space :: forall r. LensP { space :: Boolean | r } Boolean
space = lens _.space (_ { space = _ })

frame :: forall r. LensP { frame :: Time | r } Time
frame = lens _.frame (_ { frame = _ })

mkInput :: Boolean -> Boolean -> Boolean -> Boolean -> Boolean -> Time -> KeyInput
mkInput = { left: _, right: _, up:_, down: _, space:_, frame:_ }

readInput :: forall eff. Eff (timer :: Timer, dom :: DOM | eff) (Signal KeyInput)
readInput = do
   l <- keyPressed leftKeyCode
   r <- keyPressed rightKeyCode
   u <- keyPressed upKeyCode
   d <- keyPressed downKeyCode
   s <- limitShooting
   f <- animationFrame
   pure $ mkInput <$> l <*> r <*> u <*> d <*> s <*> f
