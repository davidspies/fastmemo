{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Char () where

import Data.Bits (complement, countLeadingZeros)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.UTF8 as UTF8
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Util (memoizeFixedLen)
import Data.Function.FastMemo.Word ()
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Word (Word8)

-- We want ASCII Chars to require only a single Vector lookup, so let's encode as UTF-8
instance Memoizable Char where
  memoize f = memoize (f . codePointToChar) . charToCodePoint

newtype CodePoint = CodePoint {getCodePoint :: NonEmpty Word8}

-- In UTF-8, the first byte of a codepoint tells us how many more bytes that codepoint contains.
-- We can use this fact to reduce lookups.
instance Memoizable CodePoint where
  memoize f =
    let f' = memoize (\w -> memoizeFixedLen (extraBytes w) (f . CodePoint . (w :|)))
     in \(CodePoint (w :| ws)) -> f' w ws

extraBytes :: Word8 -> Int
extraBytes x = case countLeadingOnes x of
  0 -> 0
  n -> n - 1

countLeadingOnes :: Word8 -> Int
countLeadingOnes = countLeadingZeros . complement

charToCodePoint :: Char -> CodePoint
charToCodePoint = CodePoint . NonEmpty.fromList . ByteString.unpack . UTF8.fromString . (: [])

codePointToChar :: CodePoint -> Char
codePointToChar = head . UTF8.toString . ByteString.pack . NonEmpty.toList . getCodePoint
