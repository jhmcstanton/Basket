{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}

module FRP.Basket where

import Data.HList hiding ((#))
import FRP.Basket.Signals

import Control.Monad.Fix 
import System.CPUTime 

{- |
  These are the entry point functions providing various types
  of clocking (discrete vs 'continuous' or user provided)
-}

sampleContinuously :: IO a -> Signal s a (b, Bool) -> HList s -> (b -> IO ()) -> IO ()
sampleContinuously = sample getCPUTime

sampleDiscretely :: Time -> IO a -> Signal s a (b, Bool) -> HList s -> (b -> IO ()) -> IO ()
sampleDiscretely dt sampler sf initState sync = 
  do startTime <- getCPUTime
     let op s tp = 
           do currentTime <- getCPUTime
              if currentTime - tp < dt 
              then op s tp
              else do systemIn <- sampler
                      let ((b, complete), s') = runSignal sf (currentTime - startTime) s systemIn
                      if complete then sync b else sync b >> op s' currentTime
     op initState 0

sample :: IO Time -> IO a -> Signal s a (b, Bool) -> HList s -> (b -> IO ()) -> IO ()
sample timeSampler sampler sf initState sync = 
  do startTime <- timeSampler
     let op s = do currentTime <- timeSampler
                   signalIn    <- sampler
                   let ((b, complete), s') = runSignal sf (currentTime - startTime) s signalIn
                   if complete then (sync b) else (sync b) >> op s'
     op initState                 
