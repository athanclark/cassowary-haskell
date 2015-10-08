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
import Linear.Class
import Data.Set.Class as Sets
import Data.Set.Unordered.Unique (UUSet (..))

import           Data.Key as Key
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Data.Foldable
import Control.Monad
import Control.Arrow

import Test.QuickCheck


data Tableau = Tableau
  { unrestricted      :: Map.Map String     (IneqStdForm Rational) -- ^ Unrestricted constraints with basic feasible solutions
  , unrestrictedSlack :: IntMap.IntMap      (IneqStdForm Rational) -- ^ Unrestricted constraints with a unique slack variable
  , restricted        :: Map.Map LinVarName (IneqStdForm Rational) -- ^ Restricted constraints with basic feasible solutions
  , restrictedSlack   :: IntMap.IntMap      (IneqStdForm Rational) -- ^ Restricted constraints with a unique slack variable
  } deriving (Show, Eq)

-- For generating the correct tableaus
newtype GenDisjointKey k a = GenDisjointKey {unGenDisjointKey :: [(k,a)]}
  deriving (Show, Eq, HasSize)

instance (Arbitrary k, Arbitrary a, HasNames k, HasNames a) => Arbitrary (GenDisjointKey k a) where
  arbitrary = sized go
    where
      go s = do
        n <- choose (0,s)
        xs <- replicateM n $ do
          a <- arbitrary
          k <- arbitrary `suchThat` (\k -> not $
                    fromFoldable (names k)
                  `isSubsetOf` (fromFoldable (names a) :: UUSet LinVarName))
          return (k,a)
        return $ GenDisjointKey xs

instance Arbitrary Tableau where
  arbitrary = do
    (GenDisjointKey us)  :: GenDisjointKey String (IneqStdForm Rational) <-
      arbitrary `suchThat` (\x -> size x > 0 && size x < 100)

    (GenDisjointKey us') :: GenDisjointKey Int (IneqStdForm Rational) <-
      arbitrary `suchThat` (\x -> size x > 0 && size x < 100)

    (GenDisjointKey rs)  :: GenDisjointKey LinVarName (IneqStdForm Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey rs) ->
        size rs > 0 && size rs < 100 && not (Map.keysSet (Map.fromList rs)
                                      `isSubsetOf` Map.keysSet (Map.mapKeys VarMain $ Map.fromList us)))
    (GenDisjointKey rs') :: GenDisjointKey Int (IneqStdForm Rational) <-
      arbitrary `suchThat` (\(GenDisjointKey rs') ->
        size rs' > 0 && size rs' < 100 && not (IntMap.keysSet (IntMap.fromList rs')
                                        `isSubsetOf` IntMap.keysSet (IntMap.fromList us')))
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
  in basicU `union` slackU `union` basicR `union` slackR

unrestrictedMainVars :: Tableau -> UUSet LinVarName
unrestrictedMainVars (Tableau us us' _ _) =
  unions [ fromFoldable $ VarMain <$> Map.keys us
         , fromFoldable $ concatMap names us
         , fromFoldable $ concatMap names us'
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
