{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  , FlexibleInstances
  , DeriveFoldable
  , DeriveTraversable
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Weights
  ( Weight (..)
  , makeWeight
  , withWeight
  , compressWeight
  , weightIsZero
  , addWeight
  , subWeight
  , addMapWeight
  , subMapWeight
  , leftMultWeight
  , rightMultWeight
  ) where

import Linear.Grammar

import Data.These
import Data.Align
import Data.Foldable
import Data.Monoid
import qualified Data.Vector as V
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import Test.QuickCheck


onBoth :: (a -> a -> a) -> These a a -> a
onBoth _ (This x) = x
onBoth _ (That y) = y
onBoth f (These x y) = f x y

-- | Weighted value of type @a@.
newtype Weight a = Weight {getWeight :: V.Vector a}
  deriving (Show, Functor, Applicative, Monad, Alternative, MonadPlus, Foldable, Traversable)

instance (Eq a, Num a, Arbitrary a) => Arbitrary (Weight a) where
  arbitrary = sized go
    where
      go s = do n <- choose (0,s)
                xs <- replicateM n arbitrary
                return . Weight . V.fromList $ foldr noZeroTail [] xs
      noZeroTail 0 [] = []
      noZeroTail z zs = z:zs

makeWeight :: Num a => a -> Int -> Weight a
makeWeight x w | w < 0 = error "Attempted to create weight with negative value."
               | otherwise = Weight $ V.replicate w 0 <> V.singleton x

-- | Applies @makeWeight@ to each coefficient.
withWeight :: Num a => IneqStdForm k a -> Int -> IneqStdForm k (Weight a)
withWeight x w = fmap (flip makeWeight w) x


instance (Eq a, Num a) => Eq (Weight a) where
  (Weight x) == (Weight y) = getAll . fold $ alignWith (All . these (== 0) (== 0) (==)) x y

instance (Ord a, Num a) => Ord (Weight a) where
  compare (Weight xs) (Weight ys) = fold $ V.zipWith compare xs ys

instance (Eq a, Num a) => Num (Weight a) where
  (+)     = addWeight
  (-)     = subWeight
  xs * ys = (compressWeight xs) `leftMultWeight` ys
  negate  = fmap negate
  abs     = fmap abs
  signum  = fmap signum
  fromInteger i = Weight $ V.singleton $ fromInteger i

compressWeight :: Num a => Weight a -> a
compressWeight (Weight xs) = V.sum xs

weightIsZero :: (Eq a, Num a) => Weight a -> Bool
weightIsZero (Weight xs) = V.all (== 0) xs

addWeight :: (Eq a, Num a) => Weight a -> Weight a -> Weight a
addWeight (Weight xs) (Weight ys) = Weight $
    let xs' = V.toList xs
        ys' = V.toList ys
    in V.fromList . foldr go [] $ alignWith (onBoth (+)) xs' ys'
  where
    go 0 [] = []
    go z zs = z:zs

addMapWeight :: (Ord k, Eq a, Num a) => Map.Map k (Weight a) -> Map.Map k (Weight a) -> Map.Map k (Weight a)
addMapWeight xs ys = Map.filter (not . null) $ Map.unionWith addWeight xs ys

subWeight :: (Eq a, Num a) => Weight a -> Weight a -> Weight a
subWeight xs ys = addWeight xs (fmap negate ys)

subMapWeight :: (Ord k, Eq a, Num a) => Map.Map k (Weight a) -> Map.Map k (Weight a) -> Map.Map k (Weight a)
subMapWeight xs ys = Map.filter (not . null) $ Map.unionWith subWeight xs ys

leftMultWeight :: Num a => a -> Weight a -> Weight a
leftMultWeight x xs = fmap (x *) xs

rightMultWeight :: Num a => Weight a -> a -> Weight a
rightMultWeight = flip leftMultWeight

