{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.ByteString where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.Word ()

instance Memoizable SBS.ByteString where
  memoize f = memoize (f . SBS.pack) . SBS.unpack

instance Memoizable LBS.ByteString where
  memoize f = memoize (f . LBS.pack) . LBS.unpack
