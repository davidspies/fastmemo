{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.DList where

import Data.DList (DList)
import Data.Function.FastMemo.Class (Memoizable)
import Data.Function.FastMemo.List (AsList (..))

deriving via AsList (DList a) instance Memoizable a => Memoizable (DList a)
