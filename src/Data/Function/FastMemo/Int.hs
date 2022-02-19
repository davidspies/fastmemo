{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Int () where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Word ()
import Data.Int
import Data.Word

deriving via IntegralConversion Int Word instance Memoizable Int

deriving via IntegralConversion Int8 Word8 instance Memoizable Int8

deriving via IntegralConversion Int16 Word16 instance Memoizable Int16

deriving via IntegralConversion Int32 Word32 instance Memoizable Int32

deriving via IntegralConversion Int64 Word64 instance Memoizable Int64

newtype IntegralConversion a b = IntegralConversion {getIntegralConversion :: a}

instance (Integral a, Integral b, Memoizable b) => Memoizable (IntegralConversion a b) where
  memoize f =
    memoize (f . IntegralConversion . fromIntegral)
      . (fromIntegral :: a -> b)
      . getIntegralConversion
