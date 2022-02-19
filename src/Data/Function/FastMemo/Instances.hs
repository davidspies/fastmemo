{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Instances () where

import Data.Complex (Complex)
import Data.Function.FastMemo.Char ()
import Data.Function.FastMemo.Class (Memoizable)
import Data.Function.FastMemo.Int ()
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy)
import Data.Version (Version)
import Data.Void (Void)

instance Memoizable Bool

instance Memoizable a => Memoizable (Maybe a)

instance (Memoizable a, Memoizable b) => Memoizable (Either a b)

instance Memoizable Void

instance Memoizable ()

instance Memoizable (Proxy a)

instance Memoizable Ordering

instance Memoizable a => Memoizable (Complex a)

instance Memoizable Version

instance Memoizable a => Memoizable (Identity a)

instance (Memoizable a, Memoizable b, Memoizable c) => Memoizable (a, b, c)

instance (Memoizable a, Memoizable b, Memoizable c, Memoizable d) => Memoizable (a, b, c, d)

instance (Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e) => Memoizable (a, b, c, d, e)

instance (Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e, Memoizable f) => Memoizable (a, b, c, d, e, f)

instance (Memoizable a, Memoizable b, Memoizable c, Memoizable d, Memoizable e, Memoizable f, Memoizable g) => Memoizable (a, b, c, d, e, f, g)
