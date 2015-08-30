{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  #-}

module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Class

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Foldable


-- | Basic-normal form tableau, polymorphic in the basic variable type, and the
-- coefficient type used in each equation.
newtype BNFTableau a b = BNFTableau
  { unBNFTablaeu :: Map.Map a (IneqStdForm b)
  } deriving (Show, Eq)

deriving instance (Ord a) => Monoid (BNFTableau a b)

data Tableau b = Tableau
  { unrestricted :: (BNFTableau String b,     IntMap.IntMap (IneqStdForm b)) -- ^ Unrestricted constraints
  , restricted   :: (BNFTableau LinVarName b, IntMap.IntMap (IneqStdForm b)) -- ^ Restricted constraints
  , urVars       :: [String] -- ^ All unrestricted variable names
  } deriving (Show, Eq)

basicFeasibleSolution :: BNFTableau a b -> Map.Map a Rational
basicFeasibleSolution (BNFTableau solutions) =
  fmap constVal solutions

-- | Assumes all @VarMain@ to be @>= 0@
makeRestrictedTableau :: ( Foldable f
                         , HasNegate b
                         ) => f (IneqStdForm b) -> Tableau b
makeRestrictedTableau xs =
  Tableau ( BNFTableau Map.empty
          , mempty )
          ( BNFTableau Map.empty
          , makeSlackVars xs )
          []

makeUnrestrictedTableau :: ( Foldable f
                           , HasNegate b
                           ) => f (IneqStdForm b) -> Tableau b
makeUnrestrictedTableau xs =
  Tableau ( BNFTableau Map.empty
          , makeSlackVars xs )
          ( BNFTableau Map.empty
          , mempty )
          (concatMap names xs)
