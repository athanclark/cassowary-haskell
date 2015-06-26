{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Linear.Class where


class CanAddTo a b r | a b -> r where
  (.+.) :: a -> b -> r

infixr 8 .+.

class CanSubTo a b r | a b -> r where
  (.-.) :: a -> b -> r

infixl 8 .-.

class CanMultiplyTo a b r | a b -> r where
  (.*.) :: a -> b -> r

infixr 9 .*.

class CanDivideTo a b r | a b -> r where
  (./.) :: a -> b -> r

infixl 9 ./.
