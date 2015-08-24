{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  #-}

module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.List (nub)
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Control.Applicative


newtype BNFTableau a b = BNFTableau
  { unBNFTablaeu :: Map.Map a (IneqStdForm b)
  } deriving (Show, Eq)

deriving instance (Ord a) => Monoid (BNFTableau a b)

data Tableau b = Tableau
  { unrestricted :: (BNFTableau String b,     IntMap.IntMap (IneqStdForm b)) -- ^ Unrestricted constraints include at least one of @urVars@.
  , restricted   :: (BNFTableau LinVarName b, IntMap.IntMap (IneqStdForm b))
  , urVars       :: [String]
  } deriving (Show, Eq)

basicFeasibleSolution :: BNFTableau a Rational -> Map.Map a Rational
basicFeasibleSolution (BNFTableau solutions) =
  fmap constVal solutions

-- | Assumes all @VarMain@ to be @>= 0@
makeRestrictedTableau :: IntMap.IntMap IneqExpr -> Tableau Rational
makeRestrictedTableau xs =
  Tableau ( BNFTableau Map.empty
          , mempty )
          ( BNFTableau Map.empty
          , makeSlackVars $ standardForm <$> xs )
          []

makeUnrestrictedTableau :: IntMap.IntMap IneqExpr -> Tableau Rational
makeUnrestrictedTableau xs =
  Tableau ( BNFTableau Map.empty
          , makeSlackVars $ standardForm <$> xs )
          ( BNFTableau Map.empty
          , mempty )
          (concatMap names xs)

-- | Basic Feasible Solution
remainingBasics :: (Tableau Rational, Equality Rational) -> Map.Map String Rational
remainingBasics (Tableau (BNFTableau bus,us) (BNFTableau sus,ss) _, f) =
  let mkNew :: ( HasVariables a
               , HasConstant (a Rational)
               ) => a Rational -> [(String, Maybe Rational)] -- basic feasible pairs
      mkNew x = Map.toList $ Map.mapKeys unLinVarName $
        const (Just $ constVal x) <$> unLinVarMap (vars x :: LinVarMap Rational)
      allVars = foldr go mempty $ concatMap mkNew $
        EquStd f : IntMap.elems us
                ++ Map.elems bus
                ++ IntMap.elems ss
                ++ Map.elems sus -- all constraints
  in fromJust <$> Map.filter isJust allVars
  where
    go :: (String, Maybe Rational)
       -> Map.Map String (Maybe Rational)
       -> Map.Map String (Maybe Rational)
    go (k,v) acc = case Map.lookup k acc of
      Just (Just _) -> Map.update (const $ Just Nothing) k acc
      Just Nothing -> acc -- need to get unique entities
      Nothing -> Map.insert k v acc
