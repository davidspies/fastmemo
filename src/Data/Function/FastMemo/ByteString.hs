{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Function.FastMemo.ByteString () where

import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy as LBS
import Data.Function.FastMemo.Class (Memoizable (..))
import Data.Function.FastMemo.List (AsList (..))
import Data.Function.FastMemo.Word ()

deriving via (AsList SBS.ByteString) instance Memoizable SBS.ByteString

deriving via (AsList LBS.ByteString) instance Memoizable LBS.ByteString
