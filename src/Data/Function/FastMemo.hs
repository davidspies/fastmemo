{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo (Memoizable (..), memoizeFixedLen) where

import Data.Function.FastMemo.ByteString ()
import Data.Function.FastMemo.Char ()
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Instances ()
import Data.Function.FastMemo.Int ()
import Data.Function.FastMemo.Integer ()
import Data.Function.FastMemo.Natural ()
import Data.Function.FastMemo.Ratio ()
import Data.Function.FastMemo.Util (memoizeFixedLen)
import Data.Function.FastMemo.Vector ()
import Data.Function.FastMemo.Word ()
