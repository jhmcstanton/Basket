module FRP.Basket.Signals where

import Control.Applicative

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
--  return = pure
  
