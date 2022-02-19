{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Examples where

import Data.Function.FastMemo (Memoizable, memoize)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data Color = Color {red :: Word8, green :: Word8, blue :: Word8}
  deriving (Generic, Memoizable)

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Generic, Memoizable)

fibonacci :: Natural -> Natural
fibonacci = memoize $ \i -> case i of
  0 -> 0
  1 -> 1
  _ -> fibonacci (i - 1) + fibonacci (i - 2)
