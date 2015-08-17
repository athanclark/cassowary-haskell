{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Weights where

import Linear.Class

import qualified Data.Map as Map

import Control.Applicative
import Control.Monad


data These a b = This a
               | That b
               | These a b
  deriving (Show, Eq)

alignWith :: (These a b -> c) -> [a] -> [b] -> [c]
alignWith f xs [] = (f . This) <$> xs
alignWith f [] ys = (f . That) <$> ys
alignWith f (x:xs) (y:ys) = f (These x y) : alignWith f xs ys

onBoth :: (a -> a -> a) -> These a a -> a
onBoth _ (This x) = x
onBoth _ (That y) = y
onBoth f (These x y) = f x y


newtype Weight a = Weight {unWeight :: [a]}
  deriving (Show, Functor, Applicative, Monad, Alternative, MonadPlus)

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

instance CanAddTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .+. (Weight y) = Weight $ alignWith (onBoth (.+.)) x y

instance Monoid (Weight Rational) where
  mappend = (.+.)
  mempty = Weight []

instance HasZero (Weight Rational) where
  zero' = Weight []

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

instance CanDivideTo Rational (Weight Rational) Rational where
  x ./. (Weight y) = x ./. sum y
