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
  ) where

import Linear.Class
import Linear.Grammar

import Data.These
import Data.Align
import Control.Applicative
import Control.Monad
import Test.QuickCheck


onBoth :: (a -> a -> a) -> These a a -> a
onBoth _ (This x) = x
onBoth _ (That y) = y
onBoth f (These x y) = f x y

-- | Weighted value of type @a@.
newtype Weight a = Weight {unWeight :: [a]}
  deriving (Show, Functor, Applicative, Monad, Alternative, MonadPlus, Arbitrary)

makeWeight :: Rational -> Int -> Weight Rational
makeWeight x w | w < 0 = error "Attempted to create weight with negative value."
               | otherwise = Weight $ replicate w 0 ++ [x]

-- | Applies @makeWeight@ to each coefficient after turning the input into
-- @standardForm@.
withWeight :: IneqExpr -> Int -> IneqStdForm (Weight Rational)
withWeight x w = mapVars (mapCoeffs $ flip makeWeight w) $ standardForm x


instance (Eq a, Num a) => Eq (Weight a) where
  (Weight xs') == (Weight ys') = go xs' ys'
    where
      go [] [] = True
      go xs [] = go xs [0]
      go [] ys = go [0] ys
      go (x:xs) (y:ys) = x == y && go xs ys

instance (Ord a, Num a) => Ord (Weight a) where
  compare (Weight xs') (Weight ys') = go xs' ys'
    where
      go [] [] = EQ
      go xs [] = go xs [0]
      go [] ys = go [0] ys
      go (x:xs) (y:ys) = case compare x y of
        EQ -> go xs ys
        r -> r

-- Arithmetic Instances

instance CanAddTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .+. (Weight y) = Weight $ alignWith (onBoth (.+.)) x y

instance Monoid (Weight Rational) where
  mappend = (.+.)
  mempty = Weight []

instance HasZero (Weight Rational) where
  zero' = Weight []

instance IsZero (Weight Rational) where
  isZero' (Weight xs) = null xs || all isZero' xs

instance HasOne (Weight Rational) where
  one' = Weight $ repeat 1

instance HasNegOne (Weight Rational) where
  negone' = Weight $ repeat (-1)

instance HasNegate (Weight Rational) where
  negate' (Weight xs) = Weight $ fmap negate xs

instance CanSubTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .-. (Weight y) = Weight $ alignWith (onBoth (.-.)) x y

instance CanSubTo Rational (Weight Rational) Rational where
  x .-. (Weight y) = x - sum y

instance CanMultiplyTo Rational (Weight Rational) (Weight Rational) where
  x .*. y = (x .*.) <$> y

instance CanMultiplyTo (Weight Rational) Rational (Weight Rational) where
  x .*. y = (.*. y) <$> x

instance CanMultiplyTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .*. y = sum x .*. y

divRWR :: Rational -> Weight Rational -> Rational
divRWR x (Weight ys) = x / sum ys

divRWRMaybe :: Rational -> Weight Rational -> Maybe Rational
divRWRMaybe x (Weight ys) | sum ys == 0 = Nothing
                          | otherwise   = Just $ x / sum ys

instance CanDivideTo Rational (Weight Rational) Rational where
  (./.) = divRWR

divRWW :: Rational -> Weight Rational -> Weight Rational
divRWW x (Weight ys) = Weight $ fmap (x /) ys

divRWWMaybe :: Rational -> Weight Rational -> Weight (Maybe Rational)
divRWWMaybe x (Weight ys) = Weight $ fmap go ys
  where
    go y | y == 0    = Nothing
         | otherwise = Just $ x / y

instance CanDivideTo Rational (Weight Rational) (Weight Rational) where
  (./.) = divRWW

divWWW :: Weight Rational -> Weight Rational -> Weight Rational
divWWW (Weight xs) (Weight ys) = Weight $ zipWith (/) (xs ++ [0..]) ys

divWWWMaybe :: Weight Rational -> Weight Rational -> Weight (Maybe Rational)
divWWWMaybe (Weight xs) (Weight ys) = Weight $ zipWith go (xs ++ [0..]) ys
  where
    go x y | y == 0    = Nothing
           | otherwise = Just $ x / y

instance CanDivideTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (./.) = divWWW
