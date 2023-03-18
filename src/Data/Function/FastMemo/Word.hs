{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Word () where

import Data.Bits
import Data.Foldable (foldl')
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Util (memoizeFixedLen)
import qualified Data.Vector as V
import Data.Word

instance Memoizable Word8 where
  memoize f =
    let values = f <$> V.fromList [0x00 .. 0xff]
     in \i -> values V.! fromIntegral i

deriving via MemoWord Word instance Memoizable Word

deriving via MemoWord Word16 instance Memoizable Word16

deriving via MemoWord Word32 instance Memoizable Word32

deriving via MemoWord Word64 instance Memoizable Word64

newtype MemoWord a = MemoWord a

instance (FiniteBits a, Integral a) => Memoizable (MemoWord a) where
  memoize f = memoizeFixedLen (byteLen (0 :: a)) (f . fromBytes) . toBytes

byteLen :: FiniteBits a => a -> Int
byteLen x = (finiteBitSize x + 7) `quot` 8

toBytes :: (FiniteBits a, Integral a) => MemoWord a -> [Word8]
toBytes (MemoWord x) = [fromIntegral (x `shiftR` i) | i <- [s0, s0 - 8 .. 0]]
  where
    s0 = (byteLen x - 1) * 8

fromBytes :: (Bits a, Num a) => [Word8] -> MemoWord a
fromBytes = MemoWord . foldl' (\acc x -> acc `shiftL` 8 .|. fromIntegral x) 0
