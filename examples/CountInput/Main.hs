{-# LANGUAGE DataKinds, TypeFamilies, 
             FlexibleContexts #-}

module Main where

import FRP.Basket
import FRP.Basket.Signals
import FRP.Basket.Signals.Common

import Data.Map.Strict
import Data.HList

import GHC.IO.Handle
import System.IO

main :: IO ()
main = sampleContinuously sampler processor (HCons empty HNil) out

sampler :: IO Char
sampler = hSetBuffering stdin NoBuffering >> getChar

processor :: Signal '[(Map Char Int)] Char ((Time, Map Char Int), Bool)
processor = runUntil 100 $ mkSignal $ \t a s -> let s' = alter update a s in ((t, s'), s')
  where
    update :: Maybe Int -> Maybe Int
    update Nothing   = Just 1
    update (Just n)  = Just $ n + 1

out :: (Time, Map Char Int) -> Bool -> IO ()
out p _ = print p
