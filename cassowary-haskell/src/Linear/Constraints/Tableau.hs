{-# LANGUAGE
    DeriveFunctor
  #-}

module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Grammar.Class

import qualified Data.Map as Map


-- * Basic Normal Form


newtype BNFTableau a = BNFTableau
  { unBNFTablaeu :: Map.Map LinVarName a
  } deriving (Show, Eq, Functor)

data Tableau a = Tableau
  { unrestricted :: (BNFTableau a, [a])
  , restricted   :: (BNFTableau a, [a])
  , positives    :: [LinVarName]
  } deriving (Show, Eq, Functor)
