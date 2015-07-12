{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Weights where

import Linear.Class

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
  deriving (Show, Eq, Functor, Applicative, Monad, Alternative, MonadPlus)

instance CanAddTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .+. (Weight y) = Weight $ alignWith (onBoth (.+.)) x y

instance CanSubTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .-. (Weight y) = Weight $ alignWith (onBoth (.-.)) x y


instance CanMultiplyTo Rational (Weight Rational) (Weight Rational) where
  x .*. y = (x .*.) <$> y

instance CanMultiplyTo (Weight Rational) Rational (Weight Rational) where
  x .*. y = (.*. y) <$> x

instance CanMultiplyTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .*. y = sum x .*. y

instance CanDivideTo Rational (Weight Rational) Rational where
  x ./. (Weight y) = x ./. sum y
