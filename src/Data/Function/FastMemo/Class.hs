{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Data.Function.FastMemo.Class (Memoizable (..)) where

import GHC.Generics

class Memoizable a where
  memoize :: (a -> b) -> a -> b
  default memoize :: (Generic a, GMemoize (Rep a)) => (a -> b) -> a -> b
  memoize f = gMemoize (f . to) . from

class GMemoize a where
  gMemoize :: (a p -> b) -> a p -> b

instance GMemoize f => GMemoize (M1 i c f) where
  gMemoize f = gMemoize (f . M1) . unM1

instance GMemoize V1 where
  gMemoize _f = \case {}

instance GMemoize U1 where
  gMemoize f = let fu = f U1 in \U1 -> fu

instance Memoizable c => GMemoize (K1 i c) where
  gMemoize f = memoize (f . K1) . unK1

instance (GMemoize a, GMemoize b) => GMemoize (a :*: b) where
  gMemoize f =
    let f' = gMemoize (\x -> gMemoize (\y -> f (x :*: y)))
     in \(x :*: y) -> f' x y

instance (GMemoize a, GMemoize b) => GMemoize (a :+: b) where
  gMemoize f =
    let fL = gMemoize (f . L1)
        fR = gMemoize (f . R1)
     in \case
          L1 x -> fL x
          R1 x -> fR x

instance (Memoizable a, Memoizable b) => Memoizable (a, b)
