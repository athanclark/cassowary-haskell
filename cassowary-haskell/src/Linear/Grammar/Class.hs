{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  #-}

module Linear.Grammar.Class where


class HasVariables a b | a -> b where
  names :: a -> [String]
  mapNames :: ([String] -> [String]) -> a -> a
  vars :: a -> b
  mapVars :: (b -> b) -> a -> a

class HasCoefficients a where
  coeffVals :: a -> [Rational]
  mapCoeffs :: ([Rational] -> [Rational]) -> a -> a

class HasConstant a where
  constant :: a -> Rational
  mapConstant :: (Rational -> Rational) -> a -> a
