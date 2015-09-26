{-# LANGUAGE DataKinds, ScopedTypeVariables, 
             TypeFamilies, FlexibleContexts #-}

module FRP.Basket.Signals.Switches where

import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow

import Data.HList 

import FRP.Basket.Signals
import FRP.Basket.Signals.Common

data Event e = Event e | NoEvent deriving (Show, Eq)

{- | Whenever the signal e occurs the handler function is called for the return value.
     The handler and the signal have differing states and the switch keeps track of both. 
-}

hSwitch :: forall s s' ss a b e n. 
          (HSplitAt n ss s s', ss ~ HAppendListR s (HAppendListR s' '[]), 
           HAppendFD s' '[] s', HAppendFD s s' ss) => 
          Signal s a (b, Event e) -> Signal s' e b -> Signal ss a b
hSwitch sf handler =
  Kleisli $ \a -> do (t, wstate) <- askAndGet                                     
                     let (sfState, hState) = hSplitAt (Proxy :: Proxy n) wstate
                     let ((b, ev), sfState') = runSignal sf t sfState a
                     case ev of
                       NoEvent -> lift (put (hConcat $ hBuild sfState' hState)) >> return b
                       Event e -> do let (b', hState') = runSignal handler t hState e
                                     lift (put (hConcat $ hBuild sfState' hState'))
                                     return b'



{- | Similar to a regular switch, but instead of having a signal function and a handler signal
     with differing state types, these are equivalent. In this case the state that the handler uses
     is the returned state of the original function call (sf) 
-}
switch :: Signal s a (b, Event e) -> Signal s e b -> Signal s a b
switch sf handler = 
  Kleisli $ \a -> do (t, s) <- askAndGet 
                     let ((b, ev), s') = runSignal sf t s a
                     case ev of
                       NoEvent -> lift (put s') >> return b
                       Event e -> do let (b', s'') = runSignal handler t s' e
                                     lift (put s'')
                                     return b'

{- | This function allows a 'regulator' to see the output of a signal along with it's 
     internal state to perform some time of regulatory transformation on its output.     
-}

hRegulator :: forall s s' ss a b c n. 
            (HSplitAt n ss s s', 
             ss ~ HAppendListR s (HAppendListR s' '[]), 
             HAppendFD s' '[] s', HAppendFD s s' ss) =>
             Signal s a b -> Signal s' (b, HList s) c -> Signal ss a c
hRegulator sf regulator = 
  Kleisli $ \a -> do (t, wstate)       <- askAndGet
                     let (sfState, rState) = hSplitAt (Proxy :: Proxy n) wstate
                     let (b, sfState') = runSignal sf t sfState a
                     let (c, rState')  = runSignal regulator t rState (b, sfState')
                     lift (put . hConcat $ hBuild sfState' rState')
                     return c

{- |
  A regulator that compares the initial state of the Signal sf it regulates to sf's output
-}

regulator :: Signal s a b -> Signal s (b, HList s) c -> Signal s a c
regulator sf regulator = 
  Kleisli $ \a -> do (t, s) <- askAndGet
                     let (b, s') = runSignal sf t s a 
                     let (c, s'') = runSignal regulator t s (b, s')
                     lift (put s'')
                     return c



parSwitch :: Signal s a (Either b c) -> Signal s b d -> Signal s c d -> Signal s a d
parSwitch main left right = main >>> (left ||| right)                     
