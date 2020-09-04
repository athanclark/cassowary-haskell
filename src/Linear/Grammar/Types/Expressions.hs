{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Grammar.Types.Expressions where

import Linear.Grammar.Types.Syntax (LinAst (..), multLin)
import Linear.Grammar.Types.Class (Constraint (..))
import Linear.Grammar.Types.Utilities (addMap)

import qualified Data.Map as Map
import Control.Monad (replicateM)
import Test.QuickCheck (Arbitrary (arbitrary), choose, suchThat)


-- | Linear expressions suited for normal and standard form.
data LinExpr var coeff const = LinExpr
  { linExprVars  :: Map.Map var coeff -- ^ The sum of variables and their coefficients
  , linExprConst :: const -- ^ In standard form, the constant in @const = sum [var * coeff, var * coeff, ...]@
  } deriving (Show, Eq, Functor, Foldable, Traversable)

-- FIXME if it's a random string, will it still be alphanumeric?
instance (Ord k, Eq a, Arbitrary k, Arbitrary a, Num a, Arbitrary c) => Arbitrary (LinExpr k a c) where
  arbitrary = do
    n <- choose (0, 1000)
    let genEntry = (,) <$> arbitrary <*> (arbitrary `suchThat` (/= 0))
    LinExpr <$> (Map.fromList <$> replicateM n genEntry) <*> arbitrary

instance (Ord k, Eq a, Num a, Num c) => Semigroup (LinExpr k a c) where
  (<>) (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (addMap vs1 vs2) (x + y)

instance (Ord k, Eq a, Num a, Num c) => Monoid (LinExpr k a c) where
  mempty = LinExpr mempty 0
  mappend = (<>)

instance Constraint LinExpr where
  getKeys (LinExpr xs _) = Map.keys xs
  getKeysSet (LinExpr xs _) = Map.keysSet xs
  getConst (LinExpr _ c) = c
  getVars (LinExpr xs _) = xs
  mapConst f (LinExpr xs c) = LinExpr xs (f c)
  mapAllVars f (LinExpr xs c) = LinExpr (f xs) c
  mapVars f (LinExpr xs c) = LinExpr (fmap f xs) c


-- | Turns 'LinAst' to 'LinExpr' - should be done /after/ 'multLin'.
addLin :: forall k a. Eq a => Num a => Ord k => LinAst k a -> LinExpr k a a
addLin = go mempty
  where
    -- fold over the LinAst, with an empty initial accumulator
    go :: LinExpr k a a -> LinAst k a -> LinExpr k a a
    go (LinExpr vs c) (EVar n) =
      let onLookup = maybe (Map.insert n 1 vs)
                       (\coeff -> Map.insert n (coeff + 1) vs)
      in  LinExpr (onLookup (Map.lookup n vs)) c
    go (LinExpr vs c) (ELit x) = LinExpr vs (c + x)
    go acc (ECoeff _ 0) = acc
    go (LinExpr vs c) (ECoeff (EVar n) x) =
      LinExpr (maybe (Map.insert n x vs)
                     (\coeff -> Map.insert n (coeff + x) vs) $
                  Map.lookup n vs) c
    go le (EAdd e1 e2) = go le e1 <> go le e2
    go _ _ = error "`addLin` was used on input not processed with `multLin`."

makeLinExpr :: Eq a => Num a => Ord k => LinAst k a -> LinExpr k a a
makeLinExpr = addLin . multLin
