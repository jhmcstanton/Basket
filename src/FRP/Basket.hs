{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}

module FRP.Basket where

import Data.HList hiding ((#))
import FRP.Basket.Signals
import FRP.Basket.Signals.Common

import Control.Monad.Fix 
import System.CPUTime 


import Data.Monoid -- This can be pulled once the integration sample is moved or removed

{- |
  These are the entry point functions providing various types
  of clocking (discrete vs 'continuous' or user provided).

  The Bool in the output functions of the form (b -> Bool -> IO ()) are used to indicate
  the final call of that function. When True any actions that need to be performed to complete
  IO should be completed.
-}

-- Uses getCPUTime to get the time then converts it to milliseconds
sampleContinuously :: IO a -> Signal s a (b, Bool) -> HList s -> (b -> Bool -> IO ()) -> IO ()
sampleContinuously = sample (getCPUTime >>= \tp -> return $ (fromInteger tp) * 1e-9)

-- Uses getCPUTime to get the time then converts it to milliseconds
sampleDiscretely :: Time -> IO a -> Signal s a (b, Bool) -> HList s -> (b -> Bool -> IO ()) -> IO ()
sampleDiscretely dt sampler sf initState sync = 
  do startTimeIO <- getCPUTime
     let startTime = (fromInteger startTimeIO) * 1e-9
     let op s tp = 
           do currentTimeIO <- getCPUTime
              let currentTime = (fromInteger currentTimeIO) * 1e-9
              if currentTime - tp < dt 
              then op s tp
              else do systemIn <- sampler
                      let ((b, complete), s') = runSignal sf (currentTime - startTime) s systemIn
                      if complete then sync b complete else sync b complete >> op s' currentTime
     op initState 0

sample :: IO Time -> IO a -> Signal s a (b, Bool) -> HList s -> (b -> Bool -> IO ()) -> IO ()
sample timeSampler sampler sf initState sync = 
  do startTime <- timeSampler
     let op s = do signalIn    <- sampler
                   currentTime <- timeSampler                   
                   let ((b, complete), s') = runSignal sf (currentTime - startTime) s signalIn
                   if complete then sync b complete else sync b complete >> op s'
     op initState                 


integrateTil :: (Monoid m, Show m) => IO m -> Time -> IO ()
integrateTil sampler t = sampleContinuously sampler 
                            (runUntil t (Signal $ \_ s m -> case s of 
                                                              (HCons m' HNil) -> let m'' = m' `mappend` m 
                                                                                 in (m'', HCons m'' HNil)))
                            (HCons mempty HNil) (\b _ -> putStrLn $ show b)

-- examples
test :: IO ()
test = sampleContinuously (return ()) (runUntil 1000 time) HNil (\b _ -> putStrLn $ show b)


