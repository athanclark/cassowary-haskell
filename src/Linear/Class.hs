{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Linear.Class where


-- * Heterogeneous Arithmetic

class CanAddTo a b r | a b -> r where
  (.+.) :: a -> b -> r

infixr 8 .+.

instance CanAddTo Rational Rational Rational where
  (.+.) = (+)


class HasZero a where
  zero' :: a

instance HasZero Rational where
  zero' = 0

class IsZero a where
  isZero' :: a -> Bool

instance IsZero Rational where
  isZero' x = x == 0

class HasOne a where
  one' :: a

instance HasOne Rational where
  one' = 1

class HasNegOne a where
  negone' :: a

instance HasNegOne Rational where
  negone' = -1


class HasNegate a where
  negate' :: a -> a

instance HasNegate Rational where
  negate' = negate


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


class CanDivideTo a b r where
  (./.) :: a -> b -> r

infixl 9 ./.

instance CanDivideTo Rational Rational Rational where
  (./.) = (/)
