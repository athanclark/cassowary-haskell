{-# LANGUAGE
    DeriveFunctor
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Linear.Constraints.Tableau where

import Linear.Grammar
import Linear.Grammar.Class

import qualified Data.Map as Map


-- * Basic Normal Form

newtype BNFTableau a = BNFTableau
  { unBNFTablaeu :: Map.Map a IneqStdForm
  } deriving (Show, Eq)

deriving instance (Ord a) => Monoid (BNFTableau a)

data Tableau = Tableau
  { unrestricted :: (BNFTableau String, [IneqStdForm])
  , restricted   :: (BNFTableau LinVarName, [IneqStdForm])
  , urVars       :: [LinVarName]
  } deriving (Show, Eq)

basicNormalSolution :: BNFTableau a -> Map.Map a Rational
basicNormalSolution (BNFTableau solutions) =
  fmap constVal solutions
