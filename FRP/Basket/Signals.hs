{-# LANGUAGE DataKinds #-}

module FRP.Basket.Signals where

import Control.Applicative
import Data.HList

type Time = Double

newtype StateSignal s a b = StateSignal {
                            runSignal :: Time -> s -> a -> (b, s)
                          } 

type Signal a b =  StateSignal () a b

mkSignal :: (Time -> a -> b) -> Signal a b
mkSignal f = StateSignal $ \t _ a -> (f t a, ())

instance Functor (StateSignal s a) where
  fmap f sig = StateSignal $ \t st a -> let (b, st') = runSignal sig t st a in (f b, st')


instance Applicative (StateSignal s a) where
  pure a = StateSignal $ \_ s _ -> (a, s)
  (StateSignal gen) <*> (StateSignal f) = StateSignal $ \t s a -> let (b, s') = f t s a 
                                                                      (g, s'') = gen t s a in (g b, s')

--instance Monad (StateSignal s a) where
  --return = pure
  --(StateSignal sf) >>= f = 

-- s :: * -> * -> * -> *
{-class Strip s where
  weave :: (Time -> st -> a -> (b, st)) -> s st a b  
  --(#)   :: (Time -> st -> a -> (b, st)) -> (Time -> st' -> b -> (c, st')) -> (Time -> (st, st') -> a -> (c, (st, st')))
  (#) :: s st a b -> s st' b c -> s (st, st') a c -}
  
(#) :: StateSignal s a b -> StateSignal s' b c -> StateSignal (s, s') a c
(StateSignal f) # (StateSignal g) = 
  StateSignal $ \t (st, st') a -> let (b, stNext)  = f t st a 
                                      (c, stNext') = g t st' b in (c, (stNext, stNext'))
