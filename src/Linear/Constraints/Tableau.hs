{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  #-}

module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Class
import Data.Set.Class as Sets
import Data.Set.Unordered.Unique (UUSet (..))

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Foldable
import Control.Arrow

import Test.QuickCheck


-- | Basic-normal form tableau, polymorphic in the basic variable type, and the
-- coefficient type used in each equation.
newtype BNFTableau a b = BNFTableau
  { unBNFTablaeu :: Map.Map a (IneqStdForm b)
  } deriving (Show, Eq, Monoid, Functor, Foldable, Traversable, HasNames)


instance ( Arbitrary a
         , Arbitrary b
         , Eq b
         , Num b
         , Ord a
         ) => Arbitrary (BNFTableau a b) where
  arbitrary = BNFTableau <$> arbitrary `suchThat`
                (\x -> size x > 0 && size x < 100)


data Tableau b = Tableau
  { unrestricted :: (BNFTableau String b,     IntMap.IntMap (IneqStdForm b)) -- ^ Unrestricted constraints
  , restricted   :: (BNFTableau LinVarName b, IntMap.IntMap (IneqStdForm b)) -- ^ Restricted constraints
  , urVars       :: [String] -- ^ All unrestricted variable names
  } deriving (Show, Eq, Functor, Foldable, Traversable)

instance ( Arbitrary b
         , Eq b
         , Num b
         ) => Arbitrary (Tableau b) where
  arbitrary = do
    us <- arbitrary
    us' <- arbitrary `suchThat` (\x -> size x > 0 && size x < 100)
    rs <- arbitrary
    rs' <- arbitrary `suchThat` (\x -> size x > 0 && size x < 100)
    return $ Tableau (us,us') (rs,rs') $
      unUUSet $ UUSet (names us) `union` UUSet (concatMap names us')


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
  Tableau ( mempty
          , mempty )
          ( mempty
          , makeSlackVars xs )
          []

makeUnrestrictedTableau :: ( Foldable f
                           , HasNegate b
                           ) => f (IneqStdForm b) -> Tableau b
makeUnrestrictedTableau xs =
  Tableau ( mempty
          , makeSlackVars xs )
          ( mempty
          , mempty )
          (concatMap names xs)
