{-# LANGUAGE TypeOperators, DataKinds, 
             ScopedTypeVariables, TypeFamilies, FlexibleContexts #-}

module FRP.Basket.Signals where

import Prelude hiding ((.), const)

import Control.Applicative
import Control.Category
import Control.Arrow
--import FRP.Basket.Aux.HList
import Data.HList hiding ((#))

type Time = Double

newtype Signal s a b = Signal {
                           runSignal :: Time -> HList s -> a -> (b, HList s)
                       }

-- both of these mkSignals type check, but the 2nd one is more easily composable via (#) than this one
mkSignal :: (Time -> s -> a -> (b, s)) -> Signal (s ': t) a b
mkSignal f = Signal $ \t w a -> case w of 
                                  (HCons s tail) -> let (b, s') = f t s a in (b, HCons s' tail) 


mkSignal' :: (Time -> s -> a -> (b, s)) -> Signal '[s] a b
mkSignal' f = Signal $ \t st a -> case st of
                                   (HCons s _) -> let (b, s') = f t s a in (b, HCons s' HNil)

(#) :: forall s s' ss a b c n. (HSplitAt n ss s s', ss ~ HAppendListR s (HAppendListR s' '[]), 
                                HAppendFD s' '[] s', HAppendFD s s' ss) => 
       Signal s  a b -> Signal s' b c -> Signal ss a c
(Signal f) # (Signal g) = Signal h where
  splitIndex = Proxy :: Proxy n
  h :: Time -> HList ss -> a -> (c, HList ss)
  h t wstate a = (c, hConcat $ hBuild fState' gState')
    where
      fState, fState' :: HList s
      gState, gState' :: HList s'
      (fState, gState) = hSplitAt splitIndex wstate 
      (b, fState')     = f t fState a 
      (c, gState')     = g t gState b 
                                       

-- need to do these proofs
instance Functor (Signal s a) where
  fmap f (Signal g) = Signal $ \t s a -> let (b, s') = g t s a in (f b, s')


instance Applicative (Signal s a) where
  pure a = Signal $ \_ x _ -> (a, x)
  (Signal f) <*> (Signal g) = Signal $ \t s a -> let (b, s' ) = g t s a 
                                                     (h, s'') = f t s' a in (h b, s'')

instance Monad (Signal s a) where
  return = pure
  (Signal f) >>= g = Signal $ \t s a -> let (b, s') = f t s a in runSignal (g b) t s' a
instance Category (Signal s) where
  id = Signal $ \_ s a -> (a, s)
  (Signal f) . (Signal g) = Signal $ \t s a -> let (b, s') = g t s a in f t s' b


-- state doesn't really compose in a reasonable way for this to actually be an arrow..
instance Arrow (Signal s) where
  arr f = Signal $ \_ s a -> (f a, s)
  first (Signal f) = Signal $ \t s (a, c) -> let (b, s') = f t s a in ((b, c), s')
  second (Signal f) = Signal $ \t s (c, a) -> let (b, s') = f t s a in ((c, b), s')
  (Signal f) *** (Signal g) = Signal $ \t s (a, c) -> let (b, s' ) = f t s a 
                                                          (d, s'') = g t s c in ((b, d), s) -- which s to use ?
  (Signal f) &&& (Signal g) = Signal $ \t s a -> let (b, s') = f t s a 
                                                     (d, s'') = g t s a in ((b, d), s) -- which s to use ?

{-


instance ArrowChoice Signal where
  left (Signal f) = Signal $ \t e -> 
                               case e of
                                 Left b  -> Left $ f t b 
                                 Right d -> Right d
  right (Signal f) = Signal $ \t e -> 
                                case e of
                                  Left d  -> Left d
                                  Right b -> Right $ f t b

instance ArrowApply Signal where
  app = Signal $ \t (sf, b) -> runSignal sf t b

instance ArrowLoop Signal where
  loop (Signal f) = Signal $ \t a -> let (b, d) = f t (a, d) in b                                       

-- Useful signal functions

const :: a -> Signal a a
const a = Signal $ \t _ -> a

identity :: Signal a a
identity = Signal $ \_ x -> x


-- This doesn't work the way it should
initLoop :: StatefulSignal s a b -> s -> Signal a b
initLoop sf s = loop $ second (const s) >>> sf
-}
