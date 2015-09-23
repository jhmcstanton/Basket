{-# LANGUAGE DataKinds, ScopedTypeVariables, 
             TypeFamilies, FlexibleContexts #-}

module FRP.Basket.Signals.Switches where

import Data.HList hiding ((#))

import FRP.Basket.Signals

data Event e = Event e | NoEvent deriving (Show, Eq)

{- | Whenever the signal e occurs the handler function is called for the return value.
     The handler and the signal have differing states and the switch keeps track of both. 
-}
switch :: forall s s' ss a b e n. 
          (HSplitAt n ss s s', ss ~ HAppendListR s (HAppendListR s' '[]), 
           HAppendFD s' '[] s', HAppendFD s s' ss) => 
          Signal s a (b, Event e) -> Signal s' e b -> Signal ss a b
switch sf handler = Signal $ \t totalState a -> 
                               let splitIndex        = Proxy :: Proxy n
                                   (sfst, handlerst) =  hSplitAt splitIndex totalState
                                   ((b, ev), sfst')  = runSignal sf t sfst a
                               in case ev of
                                    NoEvent -> (b, hConcat $ hBuild sfst' handlerst)
                                    Event e -> let (b, handlerst') = runSignal handler t handlerst e
                                               in (b, hConcat $ hBuild sfst' handlerst')

{- | Similar to a regular switch, but instead of having a signal function and a handler signal
     with differing state types, these are equivalent. In this case the state that the handler uses
     is the returned state of the original function call (sf) 
-}
monoSwitch :: Signal s a (b, Event e) -> Signal s e b -> Signal s a b
monoSwitch sf handler = Signal $ \t s a -> 
                                   let ((b, ev), s') = runSignal sf t s a
                                   in case ev of 
                                        NoEvent -> (b, s')
                                        Event e -> runSignal handler t s' e


{- | This function allows a 'regulator' to see the output of a signal along with it's 
     internal state to perform some time of regulatory transformation on its output.
-}

regulate :: forall s s' ss a b c n. 
            (HSplitAt n ss s s', 
             ss ~ HAppendListR s (HAppendListR s' '[]), 
             HAppendFD s' '[] s', HAppendFD s s' ss) =>
             Signal s a b -> Signal s' (b, HList s) c -> Signal ss a c
regulate sf regulator = 
  Signal $ \t totalState a -> let splitIndex        = Proxy :: Proxy n
                                  (sfState, rState) = hSplitAt splitIndex totalState
                                  (b, sfState')     = runSignal sf t sfState a
                                  (c, rState')      = runSignal regulator t rState (b, sfState')
                              in (c, hConcat $ hBuild sfState' rState')


monoPSwitch :: Signal s a (Either b c) -> Signal s b d -> Signal s c d -> Signal s a d
monoPSwitch main left right = 
  Signal $ \t s a -> let (r, s') = runSignal main t s a in
                     case r of
                       Left  b  -> runSignal left t s' b
                       Right c -> runSignal right t s' c
