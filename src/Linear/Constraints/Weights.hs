{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Weights
  ( Weight (..)
  , makeWeight
  , withWeight
  , compressWeight
  ) where

import Linear.Class
import Linear.Grammar

import Data.These
import Data.Align
import Data.Foldable
import Data.Monoid
import Data.Vector as V
import Control.Applicative
import Control.Monad
import Test.QuickCheck


onBoth :: (a -> a -> a) -> These a a -> a
onBoth _ (This x) = x
onBoth _ (That y) = y
onBoth f (These x y) = f x y

-- | Weighted value of type @a@.
newtype Weight a = Weight {unWeight :: Vector a}
  deriving (Show, Functor, Applicative, Monad, Alternative, MonadPlus)

instance Arbitrary a => Arbitrary (Weight a) where
  arbitrary = sized go
    where
      go s = do n <- choose (0,s)
                xs <- V.replicateM n arbitrary
                return $ Weight xs

makeWeight :: Rational -> Int -> Weight Rational
makeWeight x w | w < 0 = error "Attempted to create weight with negative value."
               | otherwise = Weight $ V.replicate w 0 <> V.singleton x

-- | Applies @makeWeight@ to each coefficient after turning the input into
-- @standardForm@.
withWeight :: IneqExpr -> Int -> IneqStdForm (Weight Rational)
withWeight x w = mapVars (mapCoeffVals $ flip makeWeight w) $ standardForm x


instance (Eq a, Num a) => Eq (Weight a) where
  (Weight x) == (Weight y) = getAll . fold $ alignWith (All . these (== 0) (== 0) (==)) x y

instance (Ord a, Num a) => Ord (Weight a) where
  compare (Weight xs) (Weight ys) = fold $ V.zipWith compare xs ys

compressWeight :: Weight Rational -> Rational
compressWeight (Weight xs) = V.sum xs

instance IsZero (Weight Rational) where
  isZero' (Weight xs) = V.all (== 0) xs

instance CanAddTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight xs) .+. (Weight ys) = Weight $
    V.filter (/= 0) $ alignWith (onBoth (+)) xs ys

instance CanSubTo (Weight Rational) (Weight Rational) (Weight Rational) where
  xs .-. ys = xs .+. (fmap negate ys)
