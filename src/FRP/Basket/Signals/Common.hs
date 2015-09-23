{-# LANGUAGE DataKinds #-}

module FRP.Basket.Signals.Common where

import FRP.Basket.Signals
import Control.Applicative
import Prelude hiding (const)

identity :: Signal '[] a a 
identity = Signal $ \_ s a -> (a, s)

const :: c -> Signal '[] a c
const = pure

-- This should usually not be needed, but is here for completeness
time :: Signal '[] a Time
time = Signal $ \t s _ -> (t, s)

-- This returns the time difference 
-- NOTE, as with all other signals this will need an initial state value provided for it (0, of course). 
deltaT :: Signal '[Time] a Time
deltaT = mkSignal $ \t s _ -> (t - s, t)

-- Maybe this should move to another module
runUntil :: Time -> Signal s a b -> Signal s a (b, Bool)
runUntil t sf = Signal $ \t' s a -> let (b, s') = runSignal sf t' s a 
                                    in if t' < t then ((b, False), s') else ((b, True), s') 
