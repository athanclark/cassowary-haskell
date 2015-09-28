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


-- | Basic-normal form tableau, polymorphic in the basic variable type, and the
-- coefficient type used in each equation.
newtype BNFTableau a b = BNFTableau
  { unBNFTablaeu :: Map.Map a (IneqStdForm b)
  } deriving (Show, Eq, Monoid, Functor, Foldable, Traversable, HasNames, HasEmpty
             , HasUnion, HasInsertWith a (IneqStdForm b)
             , HasSingletonWith a (IneqStdForm b))

type instance Key.Key (BNFTableau a) = a

lookupBNFT :: Ord a => a -> BNFTableau a b -> Maybe (IneqStdForm b)
lookupBNFT k (BNFTableau xs) = lookup k xs


bnftabKeysSet :: BNFTableau a b -> Set.Set a
bnftabKeysSet (BNFTableau xs) = Map.keysSet xs

instance ( Arbitrary a
         , Arbitrary b
         , IsZero b
         , Ord a
         ) => Arbitrary (BNFTableau a b) where
  arbitrary = BNFTableau <$> arbitrary `suchThat`
                (\x -> size x > 0 && size x < 100)


data Tableau b = Tableau
  { unrestricted :: (BNFTableau String b,     IntMap.IntMap (IneqStdForm b)) -- ^ Unrestricted constraints
  , restricted   :: (BNFTableau LinVarName b, IntMap.IntMap (IneqStdForm b)) -- ^ Restricted constraints
  } deriving (Show, Eq, Functor, Foldable, Traversable)

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

instance ( Arbitrary b
         , IsZero b
         ) => Arbitrary (Tableau b) where
  arbitrary = do
    (GenDisjointKey us)  :: GenDisjointKey String (IneqStdForm b) <-
      arbitrary `suchThat` (\x -> size x > 0 && size x < 100)
    (GenDisjointKey us') :: GenDisjointKey Int (IneqStdForm b) <-
      arbitrary `suchThat` (\x -> size x > 0 && size x < 100)
    (GenDisjointKey rs)  :: GenDisjointKey LinVarName (IneqStdForm b) <-
      arbitrary `suchThat` (\(GenDisjointKey rs) ->
        size rs > 0 && size rs < 100 && not (Map.keysSet (Map.fromList rs)
                                      `isSubsetOf` Map.keysSet (Map.mapKeys VarMain $ Map.fromList us)))
    (GenDisjointKey rs') :: GenDisjointKey Int (IneqStdForm b) <-
      arbitrary `suchThat` (\(GenDisjointKey rs') ->
        size rs' > 0 && size rs' < 100 && not (IntMap.keysSet (IntMap.fromList rs')
                                        `isSubsetOf` IntMap.keysSet (IntMap.fromList us')))
    return $ Tableau ( BNFTableau $ Map.fromList us
                , IntMap.fromList us')
                ( BNFTableau $ Map.fromList rs
                , IntMap.fromList rs')


basicFeasibleSolution :: Tableau b -> Map.Map LinVarName Rational
basicFeasibleSolution (Tableau (BNFTableau us, us') (BNFTableau rs, rs')) =
  let intMapToMap xs = Map.mapKeys (VarSlack . fromIntegral) $
                         Map.fromList $ IntMap.toList xs
      basicU = Map.mapKeys VarMain $ constVal <$> us
      slackU = intMapToMap $ constVal <$> us'
      basicR = constVal <$> rs
      slackR = intMapToMap $ constVal <$> rs'
  in basicU `union` slackU `union` basicR `union` slackR

unrestrictedMainVars :: Tableau b -> UUSet LinVarName
unrestrictedMainVars (Tableau (BNFTableau us,us') _) =
  unions [ fromFoldable $ VarMain <$> Map.keys us
         , fromFoldable $ concatMap names us
         , fromFoldable $ concatMap names us'
         ]

-- * Construction

-- | Assumes all @VarMain@ to be @>= 0@
makeRestrictedTableau :: ( Foldable f
                         , HasNegate b
                         ) => f (IneqStdForm b) -> Tableau b
makeRestrictedTableau xs =
  Tableau ( mempty
          , mempty )
          ( mempty
          , makeSlackVars xs )

makeUnrestrictedTableau :: ( Foldable f
                           , HasNegate b
                           ) => f (IneqStdForm b) -> Tableau b
makeUnrestrictedTableau xs =
  Tableau ( mempty
          , makeSlackVars xs )
          ( mempty
          , mempty )
