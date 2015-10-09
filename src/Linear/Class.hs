{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Linear.Class where


-- * Heterogeneous Arithmetic

class IsZero a where
  isZero' :: a -> Bool

instance IsZero Rational where
  isZero' x = x == 0

class CanAddTo a b r | a b -> r where
  (.+.) :: a -> b -> r

infixr 8 .+.

instance CanAddTo Rational Rational Rational where
  (.+.) = (+)

class CanSubTo a b r | a b -> r where
  (.-.) :: a -> b -> r

infixl 8 .-.

instance CanSubTo Rational Rational Rational where
  (.-.) = (-)


class CanMultiplyTo a b r | a b -> r where
  (.*.) :: a -> b -> r

infixr 9 .*.

instance CanMultiplyTo Rational Rational Rational where
  (.*.) = (*)


