{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , GeneralizedNewtypeDeriving
  , RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Grammar.Types.Inequalities where

import Linear.Grammar.Types.Expressions (LinExpr (..))
import Linear.Grammar.Types.Class (Constraint (..))

import qualified Data.Map as Map
import Test.QuickCheck (Arbitrary (arbitrary), oneof)


-- | User-level inequalities
data IneqExpr k a c =
    -- | An equality equation - @foo == bar@
    EquExpr (LinExpr k a c) (LinExpr k a c)
  | -- | A "less than or equal to" equation - @foo <= bar@
    LteExpr (LinExpr k a c) (LinExpr k a c)
  deriving (Show, Eq)

instance (Ord k, Arbitrary k, Arbitrary a, Num a, Eq a, Arbitrary c) => Arbitrary (IneqExpr k a c) where
  arbitrary = oneof
    [ EquExpr <$> arbitrary <*> arbitrary
    , LteExpr <$> arbitrary <*> arbitrary
    ]


-- | Exact equality
newtype Equality k a c = Equ {getEqu :: LinExpr k a c}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- | Less-than or equal to inequality
newtype LInequality k a c = Lte {getLte :: LinExpr k a c}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- | Internal structure for linear equations
data IneqStdForm k a c =
    EquStd {unEquStd :: Equality    k a c}
  | LteStd {unLteStd :: LInequality k a c}
  deriving (Show, Eq)

-- FIXME remove partial functions

instance (Ord k, Arbitrary k, Arbitrary a, Num a, Eq a, Arbitrary c) => Arbitrary (IneqStdForm k a c) where
  arbitrary = oneof
    [ EquStd <$> arbitrary
    , LteStd <$> arbitrary
    ]

instance Constraint IneqStdForm where
  getKeys = getKeys . ineqStdToLinExpr
  getKeysSet = getKeysSet . ineqStdToLinExpr
  getConst = getConst . ineqStdToLinExpr
  getVars = getVars . ineqStdToLinExpr
  mapConst f x = case x of
    EquStd (Equ x) -> EquStd (Equ (mapConst f x))
    LteStd (Lte x) -> LteStd (Lte (mapConst f x))
  mapAllVars f x = case x of
    EquStd (Equ x) -> EquStd (Equ (mapAllVars f x))
    LteStd (Lte x) -> LteStd (Lte (mapAllVars f x))
  mapVars f x = case x of
    EquStd (Equ x) -> EquStd (Equ (mapVars f x))
    LteStd (Lte x) -> LteStd (Lte (mapVars f x))

-- | Loses inequality data
ineqStdToLinExpr :: IneqStdForm k a c -> LinExpr k a c
ineqStdToLinExpr (EquStd (Equ x)) = x
ineqStdToLinExpr (LteStd (Lte x)) = x


-- | Turns a user-level AST to a standard from inequality.
standardForm :: forall k a c
              . Ord k
             => Eq a => Num a
             => Num c => Eq c
             => IneqExpr k a c -> IneqStdForm k a c
standardForm = go . standardize
  where
    go equation = case equation of
      EquExpr (LinExpr xs xc) (LinExpr ys yc)
        | Map.null xs && yc == 0 -> EquStd $ Equ $ LinExpr ys xc
        | Map.null ys && xc == 0 -> EquStd $ Equ $ LinExpr xs yc
      LteExpr (LinExpr xs xc) (LinExpr ys yc)
        | Map.null xs && yc == 0 -> LteStd $ Lte $ LinExpr (fmap negate ys) (negate xc) -- Ax >= M ~ -Ax <= -M
        | Map.null ys && xc == 0 -> LteStd $ Lte $ LinExpr xs yc -- Ax <= M
      _ -> error "Non-standard Ineq" -- FIXME impossible case?

    -- Standardizes user-level inequalities - to be used before 'standardForm'.
    standardize :: IneqExpr k a c -> IneqExpr k a c
    standardize ineq = case ineq of
      EquExpr x y ->
        let (x', y') = standardize' (x, y)
        in  EquExpr x' y'
      LteExpr x y ->
        let (x', y') = standardize' (x, y)
        in  LteExpr x' y'
      where
        standardize' :: (LinExpr k a c, LinExpr k a c) -> (LinExpr k a c, LinExpr k a c)
        standardize' (LinExpr xs xc, LinExpr ys yc)
          | Map.null xs = (LinExpr mempty (xc - yc), LinExpr ys 0)
          | Map.null ys = (LinExpr xs 0, LinExpr mempty (yc - xc))
          | otherwise =
              let ys' = negate <$> ys
                  -- force coefficients on one side, constant on the other
              in  (LinExpr (ys' <> xs) 0, LinExpr mempty (yc - xc))
