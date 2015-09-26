{-# LANGUAGE DataKinds, TypeFamilies #-}

module Main where

import FRP.Basket
import FRP.Basket.Signals

import Data.HList
import Data.Monoid
import Control.Arrow

main :: IO ()
main = sampleContinuously prompt sf st results

prompt :: IO String
prompt = putStrLn "Guess a number between 1 and 100" >> putStr ">> " >> getLine

sf :: Signal '[Int] String ((Int, Ordering), Bool)
sf = reader #> handler

reader :: Signal '[] String Int
reader = arr read --Signal $ \_ s a -> (read a, s)

handler :: Signal '[Int] Int ((Int, Ordering), Bool)
handler = mkSignal $ \t g s -> (((s, g `compare` 42), g == 42), s + 1)

st :: HList '[Int]
st = hEnd $ hBuild 1

results :: (Int, Ordering) -> Bool -> IO ()
results (count, ord) complete = 
  do if complete 
     then putStrLn "You guessed the number!"
     else case ord of
            LT -> putStrLn "Too low!"
            GT -> putStrLn "Too high!"
     putStrLn ("Number of guesses: " <> show count)
     putStrLn ""
