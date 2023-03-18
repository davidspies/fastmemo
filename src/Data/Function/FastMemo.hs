-- |
-- Straightforward function memoization library;
-- see [Examples](https://github.com/davidspies/fastmemo/blob/master/test/Examples.hs) for example usage
module Data.Function.FastMemo (AsList (..), Memoizable (..), memoizeFixedLen, memo1, memo2, memo3, memo4, memo5, memo6, memo7) where

import Data.Function.FastMemo.ByteString ()
import Data.Function.FastMemo.Char ()
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Containers ()
import Data.Function.FastMemo.DList ()
import Data.Function.FastMemo.Instances ()
import Data.Function.FastMemo.Int ()
import Data.Function.FastMemo.Integer ()
import Data.Function.FastMemo.List (AsList (..))
import Data.Function.FastMemo.Natural ()
import Data.Function.FastMemo.Ratio ()
import Data.Function.FastMemo.Util (memoizeFixedLen)
import Data.Function.FastMemo.Vector ()
import Data.Function.FastMemo.Word ()

memo1 :: Memoizable a => (a -> b) -> a -> b
memo1 = memoize

liftMemo :: Memoizable a => (b -> b) -> (a -> b) -> a -> b
liftMemo m f = memoize $ m . f

memo2 :: (Memoizable a, Memoizable b) => (a -> b -> c) -> a -> b -> c
memo2 = liftMemo memo1

memo3 :: (Memoizable a, Memoizable b, Memoizable c) => (a -> b -> c -> d) -> a -> b -> c -> d
memo3 = liftMemo memo2

memo4 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d) => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 = liftMemo memo3

memo5 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e) => (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> f
memo5 = liftMemo memo4

memo6 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e, Memoizable f) => (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> g
memo6 = liftMemo memo5

memo7 :: (Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e, Memoizable f, Memoizable g) => (a -> b -> c -> d -> e -> f -> g -> h) -> a -> b -> c -> d -> e -> f -> g -> h
memo7 = liftMemo memo6
