{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Vector () where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.List (AsList (..))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as UV

deriving via AsList (V.Vector a) instance Memoizable a => Memoizable (V.Vector a)

deriving via AsList (SV.Vector a) instance (SV.Storable a, Memoizable a) => Memoizable (SV.Vector a)

deriving via AsList (UV.Vector a) instance (UV.Unbox a, Memoizable a) => Memoizable (UV.Vector a)
