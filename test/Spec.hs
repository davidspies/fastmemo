{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Exception (ErrorCall (..), catch, evaluate)
import Data.Function.FastMemo
import Data.Functor (($>))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (Ratio)
import Data.Sequence (Seq)
import Data.Set (Set)
import qualified Data.Tree as Containers
import Data.Word (Word16, Word32, Word64, Word8)
import Examples (Color (Color), Tree (Leaf, Node), fibonacci)
import Numeric.Natural (Natural)
import Test.QuickCheck

prop_memoizesFib :: Property
prop_memoizesFib = forAllShrink (sized $ \size -> choose (0, size)) shrink $
  \i -> fibonacci (fromIntegral i) === allFibs !! i

allFibs :: [Natural]
allFibs = 0 : 1 : zipWith (+) allFibs (tail allFibs)

instance Arbitrary Natural where
  arbitrary = fromInteger . getNonNegative <$> arbitrary
  shrink = map (fromInteger . getNonNegative) . shrink . NonNegative . toInteger

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy

prop_memoizesId :: (Eq a, Show a, Memoizable a) => a -> Property
prop_memoizesId = let m = memoize id in \x -> m x === x

prop_memoizeFixedLenFixesLen :: (Eq a, Show a, Memoizable a) => Int -> [a] -> Property
prop_memoizeFixedLenFixesLen n =
  let m = memoizeFixedLen n id
   in \xs ->
        if length xs < n
          then expectErrorCall (Just "List too short") (m xs)
          else m xs === take n xs

expectErrorCall :: Maybe String -> a -> Property
expectErrorCall expected x =
  ioProperty $
    (evaluate x $> property False)
      `catch` \(ErrorCall msg) -> return $ maybe (property True) (msg ===) expected

prop_boolMemoizesId :: Bool -> Property
prop_boolMemoizesId = prop_memoizesId

prop_charMemoizesId :: Char -> Property
prop_charMemoizesId = prop_memoizesId

prop_stringMemoizesId :: String -> Property
prop_stringMemoizesId = prop_memoizesId

prop_integerMemoizesId :: Integer -> Property
prop_integerMemoizesId = prop_memoizesId

prop_bigIntegerMemoizesId :: Property
prop_bigIntegerMemoizesId =
  forAllShrink
    (genBigBound >>= \b -> choose (-b, b))
    shrink
    prop_memoizesId

genBigBound :: Gen Integer
genBigBound = sized $ \n -> return $ 256 ^ n

prop_naturalMemoizesId :: Natural -> Property
prop_naturalMemoizesId = prop_memoizesId

prop_bigNaturalMemoizesId :: Property
prop_bigNaturalMemoizesId =
  forAllShrink
    (genBigBound >>= \b -> (fromInteger :: Integer -> Natural) <$> choose (0, b))
    shrink
    prop_memoizesId

prop_unitMemoizesId :: () -> Property
prop_unitMemoizesId = prop_memoizesId

prop_eitherMemoizesId ::
  (Eq a, Show a, Memoizable a, Eq b, Show b, Memoizable b) => Either a b -> Property
prop_eitherMemoizesId = prop_memoizesId

prop_pairMemoizesId :: (Eq a, Show a, Memoizable a, Eq b, Show b, Memoizable b) => (a, b) -> Property
prop_pairMemoizesId = prop_memoizesId

prop_tripletMemoizesId ::
  (Eq a, Show a, Memoizable a, Eq b, Show b, Memoizable b, Eq c, Show c, Memoizable c) =>
  (a, b, c) ->
  Property
prop_tripletMemoizesId = prop_memoizesId

prop_maybeMemoizesId :: (Eq a, Show a, Memoizable a) => Maybe a -> Property
prop_maybeMemoizesId = prop_memoizesId

prop_listMemoizesId :: (Eq a, Show a, Memoizable a) => [a] -> Property
prop_listMemoizesId = prop_memoizesId

prop_intMemoizesId :: Int -> Property
prop_intMemoizesId = prop_memoizesId

prop_largeIntMemoizesId :: Large Int -> Property
prop_largeIntMemoizesId (Large i) = prop_memoizesId i

prop_int8MemoizesId :: Int8 -> Property
prop_int8MemoizesId = prop_memoizesId

prop_int16MemoizesId :: Int16 -> Property
prop_int16MemoizesId = prop_memoizesId

prop_int32MemoizesId :: Int32 -> Property
prop_int32MemoizesId = prop_memoizesId

prop_int64MemoizesId :: Int64 -> Property
prop_int64MemoizesId = prop_memoizesId

prop_wordMemoizesId :: Word -> Property
prop_wordMemoizesId = prop_memoizesId

prop_largeWordMemoizesId :: Large Word -> Property
prop_largeWordMemoizesId (Large w) = prop_memoizesId w

prop_word8MemoizesId :: Word8 -> Property
prop_word8MemoizesId = prop_memoizesId

prop_word16MemoizesId :: Word16 -> Property
prop_word16MemoizesId = prop_memoizesId

prop_word32MemoizesId :: Word32 -> Property
prop_word32MemoizesId = prop_memoizesId

prop_word64MemoizesId :: Word64 -> Property
prop_word64MemoizesId = prop_memoizesId

prop_ratioMemoizesId :: (Integral a, Memoizable a, Show a) => Ratio a -> Property
prop_ratioMemoizesId = prop_memoizesId

prop_intsetMemoizesId :: IntSet -> Property
prop_intsetMemoizesId = prop_memoizesId

prop_intmapMemoizesId :: (Eq a, Memoizable a, Show a) => IntMap a -> Property
prop_intmapMemoizesId = prop_memoizesId

prop_setMemoizesId :: (Ord a, Memoizable a, Show a) => Set a -> Property
prop_setMemoizesId = prop_memoizesId

prop_mapMemoizesId :: (Ord a, Memoizable a, Show a, Eq b, Memoizable b, Show b) => Map a b -> Property
prop_mapMemoizesId = prop_memoizesId

prop_seqMemoizesId :: (Eq a, Memoizable a, Show a) => Seq a -> Property
prop_seqMemoizesId = prop_memoizesId

prop_containerTreeMemoizesId :: (Eq a, Memoizable a, Show a) => Containers.Tree a -> Property
prop_containerTreeMemoizesId = prop_memoizesId

deriving instance Eq Color

deriving instance Show Color

instance Arbitrary Color where
  arbitrary = Color <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

prop_colorMemoizesId :: Color -> Property
prop_colorMemoizesId = prop_memoizesId

deriving instance Eq a => Eq (Tree a)

deriving instance Show a => Show (Tree a)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized $ \n -> do
    let h = (n - 1) `quot` 2
        h' = n - 1 - h
    frequency
      [ (1, Leaf <$> arbitrary),
        (max (n - 1) 0, Node <$> resize h arbitrary <*> resize h' arbitrary)
      ]
  shrink = genericShrink

prop_treeMemoizesId :: (Eq a, Show a, Memoizable a) => Tree a -> Property
prop_treeMemoizesId = prop_memoizesId

return []

main :: IO ()
main = do
  True <- $quickCheckAll
  return ()
