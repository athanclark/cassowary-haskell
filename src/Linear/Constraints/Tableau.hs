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
-- Fundamental axiom:
--
-- Isolating a variable into basic-normal form
-- (done by reciporicating its coefficient through its "defining" equation, then
-- substituting that replacement in the rest of the constraints) leaves it /invulnerable to manipulation/
-- from further variable isolation operations.
--
-- This means our algorithm has obvious and measurable traction, in terms of finding
-- a more precise \/ optimal basic-feasible solution - each time we /can/ make a pivot
-- (with Bland's rule), we get one step closer. If we can't, then we're optimal.

module Linear.Constraints.Tableau where

import Prelude hiding (lookup)

import Linear.Grammar.Types.Class (getConst, getKeysSet)
import Linear.Grammar.Types.Inequalities (IneqStdForm)
import Linear.Grammar.Types.Variables
  ( LinVarName (VarRestricted, VarMain)
  , RLinVarName (VarSlack)
  )

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Control.Arrow (first)
import Control.Monad (replicateM)

import Test.QuickCheck (Arbitrary (arbitrary), sized, suchThat, choose)



data Tableau basicK slackK coeff const = Tableau
  { tableauBasic :: Map.Map basicK (IneqStdForm basicK coeff const) -- ^ Tableau of basic variables
  , tableauSlack :: IntMap.IntMap  (IneqStdForm slackK coeff const) -- ^ Tableau of slack variables, optimized for being an @Int@ unique reference
  } deriving (Show, Eq)

-- | For generating the correct tableaus
newtype GenDisjointKey k a = GenDisjointKey {unGenDisjointKey :: [(k,a)]}
  deriving (Show, Eq)

instance
  ( Arbitrary coeff
  , Arbitrary const
  , Arbitrary k
  , Ord k
  , Num coeff
  , Eq coeff
  ) => Arbitrary (GenDisjointKey k (IneqStdForm (LinVarName k) coeff const)) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $ VarMain k `Set.member` getKeysSet a)
          pure (k,a)
        pure (GenDisjointKey xs)

instance
  ( Arbitrary coeff
  , Arbitrary const
  , Arbitrary k
  , Ord k
  , Num coeff
  , Eq coeff
  ) => Arbitrary (GenDisjointKey Int (IneqStdForm (RLinVarName k) coeff const)) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $ VarSlack k `Set.member` getKeysSet a)
          pure (k,a)
        pure (GenDisjointKey xs)

instance
  ( Arbitrary coeff
  , Arbitrary const
  , Arbitrary k
  , Ord k
  , Num coeff
  , Eq coeff
  ) => Arbitrary (GenDisjointKey (RLinVarName k) (IneqStdForm (RLinVarName k) coeff const)) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $ k `Set.member` getKeysSet a)
          pure (k,a)
        pure (GenDisjointKey xs)



instance
  ( Arbitrary coeff
  , Arbitrary const
  , Arbitrary k
  , Ord k
  , Num coeff
  , Eq coeff
  ) => Arbitrary (Tableau (RLinVarName k) (RLinVarName k) coeff const) where
  arbitrary = do
    -- FIXME ensure that basic variables are actually basic
    (GenDisjointKey us)  :: GenDisjointKey (RLinVarName k) (IneqStdForm (RLinVarName k) coeff const) <-
      arbitrary `suchThat` (\(GenDisjointKey x) -> not (null x) && length x < 100)

    (GenDisjointKey us') :: GenDisjointKey Int (IneqStdForm (RLinVarName k) coeff const) <-
      arbitrary `suchThat` (\(GenDisjointKey x) -> not (null x) && length x < 100)
    pure (Tableau (Map.fromList us) (IntMap.fromList us'))

-- | Gets the BFS of the current Tableau
basicFeasibleSolution :: Ord k => Tableau (LinVarName k) slackK coeff const -> Map.Map (LinVarName k) const
basicFeasibleSolution (Tableau basic slack) =
  let intMapToMap =
        Map.fromList . map (first (VarRestricted . VarSlack)) . IntMap.toList
      basic' = getConst <$> basic
      slack' = intMapToMap (getConst <$> slack)
  in basic' <> slack'
