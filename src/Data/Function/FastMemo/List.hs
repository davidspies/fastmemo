{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.List (AsList (..)) where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.List.NonEmpty (NonEmpty)
import GHC.Exts
import qualified GHC.Exts as IsList

instance Memoizable a => Memoizable [a]

instance Memoizable a => Memoizable (NonEmpty a)

newtype AsList t = AsList {unAsList :: t}

instance (IsList t, Memoizable (IsList.Item t)) => Memoizable (AsList t) where
  memoize f = memoize (f . AsList . IsList.fromList) . IsList.toList . unAsList
