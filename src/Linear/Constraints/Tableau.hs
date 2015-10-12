{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , FlexibleContexts
  , FlexibleInstances
  , DeriveTraversable
  , StandaloneDeriving
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}

-- |
-- ** Fundamental axioms of utility
--
-- Isolating a variable into basic-normal form
-- (done by reciporicating its coefficient through its "defining" equation, then
-- substituting that replacement in the rest of the constraints) leaves it invulnerable
-- to manipulation from further variable isolation operations.
--
-- This means our algorithm has obvious and measurable traction, in terms of finding
-- a more precise / optimal basic-feasible solution - each time we /can/ make a pivot
-- (with Bland's rule), we get one step closer. If we can't, then we're optimal.

module Linear.Constraints.Tableau where

import Prelude hiding (lookup)

import Linear.Constraints.Slack
import Linear.Grammar

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.Semigroup
import Control.Monad

import Test.QuickCheck


data Tableau k0 k1 = Tableau
  { tableauBasic :: Map.Map k0    (IneqStdForm k1 Rational)
  , tableauSlack :: IntMap.IntMap (IneqStdForm k1 Rational)
  } deriving (Show, Eq)

-- For generating the correct tableaus
newtype GenDisjointKey k a = GenDisjointKey {unGenDisjointKey :: [(k,a)]}
  deriving (Show, Eq)

instance Arbitrary (GenDisjointKey String (IneqStdForm LinVarName Rational)) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $ (VarMain k) `Set.member` ineqStdKeysSet a)
          return (k,a)
        return $ GenDisjointKey xs

instance Arbitrary (GenDisjointKey Int (IneqStdForm RLinVarName Rational)) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $ (VarSlack k) `Set.member` ineqStdKeysSet a)
          return (k,a)
        return $ GenDisjointKey xs

instance Arbitrary (GenDisjointKey RLinVarName (IneqStdForm RLinVarName Rational)) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $ k `Set.member` ineqStdKeysSet a)
          return (k,a)
        return $ GenDisjointKey xs



instance Arbitrary (Tableau RLinVarName RLinVarName) where
  arbitrary = do
    (GenDisjointKey us)  :: GenDisjointKey RLinVarName (IneqStdForm RLinVarName Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey x) -> length x > 0 && length x < 100)

    (GenDisjointKey us') :: GenDisjointKey Int (IneqStdForm RLinVarName Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey x) -> length x > 0 && length x < 100)
    return $ Tableau (Map.fromList us)
                     (IntMap.fromList us')


basicFeasibleSolution :: Tableau LinVarName k1 -> Map.Map LinVarName Rational
basicFeasibleSolution (Tableau basic slack) =
  let intMapToMap xs = Map.mapKeys (VarRestricted . VarSlack . fromIntegral) $
                         Map.fromList $ IntMap.toList xs
      basic' = ineqStdConst <$> basic
      slack' = intMapToMap $ ineqStdConst <$> slack
  in basic' <> slack'
