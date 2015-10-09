{-# LANGUAGE
    TypeFamilies
  , DeriveFunctor
  , DeriveFoldable
  , FlexibleContexts
  , DeriveTraversable
  , StandaloneDeriving
  , ScopedTypeVariables
  , GeneralizedNewtypeDeriving
  #-}

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


data Tableau = Tableau
  { unrestricted      :: Map.Map String     (IneqStdForm Rational) -- ^ Unrestricted constraints with basic feasible solutions
  , unrestrictedSlack :: IntMap.IntMap      (IneqStdForm Rational) -- ^ Unrestricted constraints with a unique slack variable
  , restricted        :: Map.Map LinVarName (IneqStdForm Rational) -- ^ Restricted constraints with basic feasible solutions
  , restrictedSlack   :: IntMap.IntMap      (IneqStdForm Rational) -- ^ Restricted constraints with a unique slack variable
  } deriving (Show, Eq)

-- For generating the correct tableaus
newtype GenDisjointKey k a = GenDisjointKey {unGenDisjointKey :: [(k,a)]}
  deriving (Show, Eq)

instance (Arbitrary k, Arbitrary a, HasNames k, HasNames a) => Arbitrary (GenDisjointKey k a) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $
                    Set.fromList (names k)
                  `Set.isSubsetOf` (Set.fromList (names a) :: Set.Set LinVarName))
          return (k,a)
        return $ GenDisjointKey xs

instance Arbitrary Tableau where
  arbitrary = do
    (GenDisjointKey us)  :: GenDisjointKey String (IneqStdForm Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey x) -> length x > 0 && length x < 100)

    (GenDisjointKey us') :: GenDisjointKey Int (IneqStdForm Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey x) -> length x > 0 && length x < 100)

    (GenDisjointKey rs)  :: GenDisjointKey LinVarName (IneqStdForm Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey rs) ->
        length rs > 0 && length rs < 100 && not (Map.keysSet (Map.fromList rs)
                                      `Set.isSubsetOf` Map.keysSet (Map.mapKeys VarMain $ Map.fromList us)))
    (GenDisjointKey rs') :: GenDisjointKey Int (IneqStdForm Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey rs') -> length rs' > 0
                                                  && length rs' < 100
                                                  && not ((Set.fromList $ fst <$> rs')
                                         `Set.isSubsetOf` (Set.fromList $ fst <$> us')))
    return $ Tableau (Map.fromList us)
                     (IntMap.fromList us')
                     (Map.fromList rs)
                     (IntMap.fromList rs')


basicFeasibleSolution :: Tableau -> Map.Map LinVarName Rational
basicFeasibleSolution (Tableau us us' rs rs') =
  let intMapToMap xs = Map.mapKeys (VarSlack . fromIntegral) $
                         Map.fromList $ IntMap.toList xs
      basicU = Map.mapKeys VarMain $ constVal <$> us
      slackU = intMapToMap $ constVal <$> us'
      basicR = constVal <$> rs
      slackR = intMapToMap $ constVal <$> rs'
  in basicU <> slackU <> basicR <> slackR

unrestrictedMainVars :: Tableau -> Set.Set LinVarName
unrestrictedMainVars (Tableau us us' _ _) =
  Set.unions [ Set.map VarMain $ Map.keysSet us
             , Set.fromList $ concatMap names us
             , Set.fromList $ concatMap names us'
             ]

-- * Construction

-- | Assumes all @VarMain@ to be @>= 0@
makeRestrictedTableau :: ( Foldable f
                         ) => f (IneqStdForm Rational) -> Tableau
makeRestrictedTableau xs =
  Tableau mempty
          mempty
          mempty
          (makeSlackVars xs)

makeUnrestrictedTableau :: ( Foldable f
                           ) => f (IneqStdForm Rational) -> Tableau
makeUnrestrictedTableau xs =
  Tableau mempty
          (makeSlackVars xs)
          mempty
          mempty
