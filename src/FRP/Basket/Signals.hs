{-# LANGUAGE TypeOperators, DataKinds, 
             ScopedTypeVariables, TypeFamilies, FlexibleContexts,
             FlexibleInstances, UndecidableInstances #-}

module FRP.Basket.Signals where

import Control.Monad.Reader
import Control.Monad.State

import Control.Applicative
import Control.Arrow
import Data.HList 


type Time = Double

type Signal s a b = Kleisli (ReaderT Time (State (HList s))) a b  

runSignal :: Signal s a b -> Time -> HList s -> a -> (b, HList s)
runSignal sf t s a = runState (runReaderT (runKleisli sf a) t) s

mkSignal :: (Time -> a -> s -> (b, s)) -> Signal (s ': ss) a b -- Signal '[s] a b
mkSignal f = 
  Kleisli $ \x -> do (t, st) <- askAndGet
                     case st of
                       (HCons s ss) -> do let (b, s') = f t x s 
                                          put $ HCons s' ss
                                          return b


-- Pronounced 'weave', this function composes Signals of differing states 
infixr #>
(#>) :: forall s s' ss a b c n. 
        (HSplitAt n ss s s', ss ~ HAppendListR s (HAppendListR s' '[]), HAppendFD s' '[] s', HAppendFD s s' ss) => 
        Signal s a b -> Signal s' b c -> Signal ss a c
f #> g = Kleisli h where
  splitIndex = Proxy :: Proxy n
--  h :: Time -> a -> HList ss -> (c, HList ss)
  h :: a -> ReaderT Time (State (HList ss)) c
  h a = do (t, wstate) <- askAndGet
           let (fState, gState) = hSplitAt splitIndex wstate
           let (b, fState') = runSignal f t fState a
           let (c, gState') = runSignal g t gState b
           put (hConcat $ hBuild fState' gState')
           return c      


askAndGetK :: Signal s a (Time, HList s)
askAndGetK = Kleisli $ \_ -> askAndGet

askAndGet :: ReaderT Time (State (HList s)) (Time, HList s)
askAndGet = lift get >>= \s -> ask >>= \t -> return (t, s)
