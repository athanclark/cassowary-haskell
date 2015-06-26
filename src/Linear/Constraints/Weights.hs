{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Linear.Constraints.Weights where

import Linear.Class


data These a b = This a
               | That b
               | These a b
  deriving (Show, Eq)

alignWith :: (These a b -> c) -> [a] -> [b] -> [c]
alignWith f xs [] = f <$> This <$> xs
alignWith f [] ys = f <$> That <$> ys
alignWith f (x:xs) (y:ys) = f <$> These x y : alignWith f xs ys


newtype Weight a = Weight {unWeight :: [a]}
  deriving (Show, Eq, Functor, Applicative, Monad, MonadFix, MonadPlus)

instance CanAddTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .+. (Weight y) = Weight $ alignWith (.+.) x y

instance CanSubTo (Weight Rational) (Weight Rational) (Weight Rational) where
  (Weight x) .-. (Weight y) = Weight $ alignWith (.-.) x y

instance CanMultiplyTo Rational (Weight Rational) (Weight Rational) where
  x .*. y = (x .*.) <$> y

instance CanMultiplyTo (Weight Rational) Rational (Weight Rational) where
  x .*. y = (.*. y) <$> x

instance CanDivideTo Rational (Weight Rational) Rational where
  x ./. (Weight y) = x ./. sum y
