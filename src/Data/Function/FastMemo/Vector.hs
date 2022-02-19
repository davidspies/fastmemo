{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Vector where

import Data.Function.FastMemo.Class (Memoizable (..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

instance Memoizable a => Memoizable (V.Vector a) where
  memoize f = memoize (f . V.fromList) . V.toList

instance (SV.Storable a, Memoizable a) => Memoizable (SV.Vector a) where
  memoize f = memoize (f . SV.fromList) . SV.toList

instance (UV.Unbox a, Memoizable a) => Memoizable (UV.Vector a) where
  memoize f = memoize (f . UV.fromList) . UV.toList
