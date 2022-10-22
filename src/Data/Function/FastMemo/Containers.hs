{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.Containers () where

import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Int ()
import Data.Function.FastMemo.List (AsList (..))
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import Data.Tree (Tree)

deriving via AsList (Map a b) instance (Ord a, Memoizable a, Memoizable b) => Memoizable (Map a b)

deriving via AsList (Set a) instance (Ord a, Memoizable a) => Memoizable (Set a)

deriving via AsList (IntMap b) instance (Memoizable b) => Memoizable (IntMap b)

deriving via AsList IntSet instance Memoizable IntSet

deriving via AsList (Seq a) instance Memoizable a => Memoizable (Seq a)

instance Memoizable a => Memoizable (Seq.ViewL a)

instance Memoizable a => Memoizable (Seq.ViewR a)

instance Memoizable a => Memoizable (Tree a)
