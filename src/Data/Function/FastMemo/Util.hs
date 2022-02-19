{-# LANGUAGE LambdaCase #-}

module Data.Function.FastMemo.Util where

import Data.Function.FastMemo.Class (Memoizable, memoize)
import GHC.Stack (HasCallStack)

-- Memoizable a function on a list whose length is predetermined.
memoizeFixedLen :: (HasCallStack, Memoizable a) => Int -> ([a] -> b) -> [a] -> b
memoizeFixedLen n f
  | n <= 0 = const (f [])
  | otherwise =
    let f' = memoize $ \x -> memoizeFixedLen (n - 1) (f . (x :))
     in \case
          [] -> error "List too short"
          x : xs -> f' x xs
