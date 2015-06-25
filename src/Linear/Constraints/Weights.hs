{-# LANGUAGE
    GeneralizedNewtypeDeriving
  #-}

module Linear.Constraints.Weights where

import Data.These
import Data.Align


newtype Weight a = Weight {unWeight :: [a]}
  deriving (Show, Eq, Functor, Monad, Applicative)

instance Num a => Num (Weight a) where
  (Weight a) + (Weight b) = Weight $ alignWith go a b
    where
      go (This x) = x
      go (That x) = x
      go (These x y) = x + y
  (Weight a) - (Weight b) = Weight $ alignWith go a b
    where
      go (This x) = x
      go (That x) = -x
      go (These x y) = x - y
  (Weight a) * (Weight b) = Weight $ alignWith go a b
    where
      go (This x) = x
      go (That x) = x
      go (These x y) = x * y
  negate (Weight a) = Weight $ negate <$> a
  abs (Weight a) = Weight $ abs <$> a
  signum (Weight a) = Weight $ signum <$> a
  fromInteger i = Weight [fromInteger i]
