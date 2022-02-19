{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Natural () where

import Data.Bits (shiftL, shiftR, (.|.))
import Data.Foldable (foldl')
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Word ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word8)
import Numeric.Natural (Natural)

instance Memoizable Natural where
  memoize f = memoize (f . wordsToNat) . natToWords

-- A slightly weird encoding that assigns unique values to each of [0], [0,0], [0,0,0]...
natToWords :: Natural -> NonEmpty Word8
natToWords = go []
  where
    go acc n =
      if n <= 0xff
        then w :| acc
        else go (w : acc) (n `shiftR` 8 - 1)
      where
        w = fromIntegral n

wordsToNat :: NonEmpty Word8 -> Natural
wordsToNat (w0 :| ws0) = foldl' (\acc w -> (acc + 1) `shiftL` 8 .|. fromIntegral w) (fromIntegral w0) ws0
