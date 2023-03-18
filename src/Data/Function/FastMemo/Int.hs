{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Int () where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Word ()
import Data.Int
import Data.Word

deriving via IntegralConversion Word Int instance Memoizable Int

deriving via IntegralConversion Word8 Int8 instance Memoizable Int8

deriving via IntegralConversion Word16 Int16 instance Memoizable Int16

deriving via IntegralConversion Word32 Int32 instance Memoizable Int32

deriving via IntegralConversion Word64 Int64 instance Memoizable Int64

newtype IntegralConversion b a = IntegralConversion a
  deriving (Enum, Num, Eq, Ord, Real, Integral)

instance (Integral a, Integral b, Memoizable b) => Memoizable (IntegralConversion b a) where
  memoize f = memoize (f . fromIntegral) . (fromIntegral :: IntegralConversion b a -> b)
