{-# LANGUAGE DataKinds #-}

module FRP.Basket.Signals.Common where

import FRP.Basket.Signals
import Control.Applicative
import Prelude hiding (const)

identity :: Signal '[] a a 
identity = Signal $ \_ s a -> (a, s)

const :: c -> Signal '[] a c
const = pure
