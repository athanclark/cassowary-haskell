{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  #-}

module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Class
import Data.Set.Class as Sets

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Foldable
import Control.Arrow


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

basicFeasibleSolution :: Tableau b -> Map.Map LinVarName Rational
basicFeasibleSolution (Tableau (BNFTableau c_u, us) (BNFTableau c_s, ss) _) =
  let intMapToMap xs = Map.fromList $ fmap (first $ VarSlack . fromIntegral) $
                          IntMap.toList $ fmap constVal xs
      c_u' = Map.mapKeys VarMain $ fmap constVal c_u
      us'  = intMapToMap us
      c_s' = fmap constVal c_s
      ss'  = intMapToMap ss
  in c_u' `union` us' `union` c_s' `union` ss'

-- * Construction

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
