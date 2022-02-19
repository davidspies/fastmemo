{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Ratio where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Ratio (Ratio, denominator, numerator, (%))

instance (Integral a, Memoizable a) => Memoizable (Ratio a) where
  memoize f = memoize (f . uncurry (%)) . (\x -> (numerator x, denominator x))
