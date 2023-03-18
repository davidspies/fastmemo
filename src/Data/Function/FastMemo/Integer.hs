{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Integer () where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Natural ()
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

instance Memoizable Integer where
  memoize f = memoize (f . signedNatToInteger) . integerToSignedNat

data Sign = NegativePlus1 | NonNegative
  deriving (Generic, Memoizable)

integerToSignedNat :: Integer -> (Sign, Natural)
integerToSignedNat i
  | i < 0 = (NegativePlus1, fromInteger (-(i + 1)))
  | otherwise = (NonNegative, fromInteger i)

signedNatToInteger :: (Sign, Natural) -> Integer
signedNatToInteger = \case
  (NegativePlus1, n) -> -(toInteger n + 1)
  (NonNegative, n) -> toInteger n
