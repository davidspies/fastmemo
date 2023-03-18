{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  deriving (IsList)

instance (IsList t, Memoizable (IsList.Item t)) => Memoizable (AsList t) where
  memoize f = memoize (f . IsList.fromList) . IsList.toList
