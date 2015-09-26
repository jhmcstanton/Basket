{-# LANGUAGE DataKinds, TypeOperators #-}

module FRP.Basket.Signals.Common where

import FRP.Basket.Signals
import Control.Applicative
import Control.Arrow
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (const)

import Data.HList

identity :: Signal '[] a a 
identity = returnA

const :: c -> Signal '[] a c
const c = arr (\_ -> c)

-- This returns the time difference 
-- NOTE, as with all other signals this will need an initial state value provided for it (0, of course). 
deltaT :: Signal '[Time] a Time
deltaT = mkSignal $ \t _ s -> (t - s, t)

-- Maybe this should move to another module

runUntil :: Time -> Signal s a b -> Signal s a (b, Bool)
runUntil t sf = sf >>> (Kleisli $ \b -> ask >>= (\t' -> if t' < t then return (b, False) else return (b, True)))

runForever :: Signal s a b -> Signal s a (b, Bool)
runForever sf = sf >>> (Kleisli $ \b -> return (b, False))

time :: Signal s a Time
time = Kleisli $ \_ -> ask
