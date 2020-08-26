{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , KindSignatures
  , DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TypeFamilies
  #-}

module Linear.Grammar.Types where

import Prelude hiding (zip, filter, lookup)

import Data.String (IsString (fromString))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad (replicateM)

import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Gen (Gen, suchThat, oneof, choose, resize, sized, scale)
import Test.QuickCheck.Instances ()


-- * User-facing API

-- | User-facing abstract syntax tree, polymorphic in the numeric type used.
data LinAst k a =
    -- | Variable names
    EVar k
  | -- | Numeric literals
    ELit a
  | -- | A literal coefficient multiplied by some abstract syntax tree
    ECoeff (LinAst k a) a
  | -- | Two abstract syntax trees added together
    EAdd (LinAst k a) (LinAst k a)
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary k) => Arbitrary (LinAst k a) where
  arbitrary = sized go
    where
      go :: Arbitrary a => Arbitrary k => Int -> Gen (LinAst k a)
      go s
        | s <= 1 = oneof
          [ EVar <$> arbitrary
          , ELit <$> arbitrary
          ]
        | otherwise = oneof
          [ EVar <$> arbitrary
          , ELit <$> arbitrary
          , ECoeff <$> scale (subtract 1) arbitrary <*> arbitrary
          , do
              n <- choose (0,s-1)
              EAdd <$> resize n arbitrary <*> resize n arbitrary
          ]

instance IsString k => IsString (LinAst k a) where
  fromString = EVar . fromString

-- | Addition operation
(.+.) :: LinAst k a -> LinAst k a -> LinAst k a
(.+.) = EAdd

-- | Multiplication operation
(.*.) :: LinAst k a -> a -> LinAst k a
(.*.) = ECoeff

-- TODO fixity?

-- * Variables

-- | Sign for error variables - converting a positive or negative number, to strictly a positive
-- number, flagged by its sign.
data ErrorSign = ErrNeg | ErrPos
  deriving (Show, Eq, Ord)

instance Arbitrary ErrorSign where
  arbitrary = boolToErrorSign <$> arbitrary
    where
      boolToErrorSign b = if b then ErrPos else ErrNeg

-- | /Restricted/ linear variable name types - only slack variables and error
-- variables are known to be @>= 0@.
data RLinVarName k =
    -- | Slack variable, with a unique name iterated by a stateful @Int@.
    VarSlack Int
  | -- | Error variable for stay and edit constraints, with its original name and the sign; either positive or negative.
    VarError k ErrorSign
  deriving (Show, Eq, Ord)

instance Arbitrary k => Arbitrary (RLinVarName k) where
  arbitrary = oneof
    [ VarSlack <$> choose (0, 1000)
    , VarError <$> arbitrary <*> arbitrary
    ]

-- | Type representing both restricted and unrestricted variables
data LinVarName k =
    VarMain k
  | VarRestricted (RLinVarName k)
  deriving (Eq, Ord)

-- | Not intended to be parsed afterward, just for debugging
instance Show k => Show (LinVarName k) where
  show x = case x of
    VarMain n -> show n
    VarRestricted (VarSlack n) -> show n
    VarRestricted (VarError n b) ->
      "error_"
        ++ show n
        ++ if b == ErrPos then "_+" else "_-"

instance Arbitrary k => Arbitrary (LinVarName k) where
  arbitrary = oneof
    [ VarMain <$> arbitrary
    , VarRestricted <$> arbitrary
    ]

-- | Linear variable
data LinVar k a = LinVar
  { varName  :: LinVarName k
  , varCoeff :: a
  } deriving (Show, Eq)

instance (Arbitrary a, Arbitrary k) => Arbitrary (LinVar k a) where
  arbitrary = LinVar <$> arbitrary <*> arbitrary

-- | Name-based ordering
instance (Eq a, Ord k) => Ord (LinVar k a) where
  compare (LinVar x _) (LinVar y _) = compare x y

-- * Variables with Coefficients

-- | Delete any @0@ entries, and union the mappings with addition.
addMap :: (Ord k, Eq a, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
addMap xs ys = Map.filter (/= 0) (Map.unionWith (+) xs ys)

subMap :: (Ord k, Eq a, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
subMap xs ys = addMap xs (negate <$> ys)

-- * Expressions

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

mergeLinExpr :: (Ord k, Eq a, Num a, Num c) => LinExpr k a c -> LinExpr k a c -> LinExpr k a c
mergeLinExpr (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (addMap vs1 vs2) (x + y)

instance (Ord k, Eq a, Num a, Num c) => Semigroup (LinExpr k a c) where
  (<>) = mergeLinExpr

instance (Ord k, Eq a, Num a, Num c) => Monoid (LinExpr k a c) where
  mempty = LinExpr mempty 0
  mappend = mergeLinExpr

linExprKeys :: LinExpr k a c -> [k]
linExprKeys (LinExpr xs _) = Map.keys xs

linExprKeysSet :: LinExpr k a c -> Set.Set k
linExprKeysSet (LinExpr xs _) = Map.keysSet xs

-- FIXME make it a functor?

linExprMapConst :: (c -> c') -> LinExpr k a c -> LinExpr k a c'
linExprMapConst f (LinExpr xs xc) = LinExpr xs (f xc)

linExprMapVars :: (Map.Map k a -> Map.Map k' a') -> LinExpr k a c -> LinExpr k' a' c
linExprMapVars f (LinExpr xs xc) = LinExpr (f xs) xc


-- * Equations

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

-- * Standard Form Equations

-- | Exact equality
newtype Equality k a c = Equ {getEqu :: LinExpr k a c}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- | Less-than or equal to inequality
newtype LInequality k a c = Lte {getLte :: LinExpr k a c}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- | Greater-than or equal to inequality
newtype GInequality k a c = Gte {getGte :: LinExpr k a c}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- | Internal structure for linear equations
data IneqStdForm k a c =
    EquStd {unEquStd :: Equality    k a c}
  | LteStd {unLteStd :: LInequality k a c}
  | GteStd {unGteStd :: GInequality k a c}
  deriving (Show, Eq)

-- FIXME remove partial functions

instance (Ord k, Arbitrary k, Arbitrary a, Num a, Eq a, Arbitrary c) => Arbitrary (IneqStdForm k a c) where
  arbitrary = oneof
    [ EquStd <$> arbitrary
    , LteStd <$> arbitrary
    , GteStd <$> arbitrary
    ]


-- | Loses inequality data
ineqStdToLinExpr :: IneqStdForm k a c -> LinExpr k a c
ineqStdToLinExpr (EquStd (Equ x)) = x
ineqStdToLinExpr (LteStd (Lte x)) = x
ineqStdToLinExpr (GteStd (Gte x)) = x

ineqStdKeys :: IneqStdForm k a c -> [k]
ineqStdKeys = linExprKeys . ineqStdToLinExpr

ineqStdKeysSet :: IneqStdForm k a c -> Set.Set k
ineqStdKeysSet = linExprKeysSet . ineqStdToLinExpr

ineqStdConst :: IneqStdForm k a c -> c
ineqStdConst = linExprConst . ineqStdToLinExpr

ineqStdVars :: IneqStdForm k a c -> Map.Map k a
ineqStdVars = linExprVars . ineqStdToLinExpr

ineqStdMapConst :: (c -> c') -> IneqStdForm k a c -> IneqStdForm k a c'
ineqStdMapConst f (EquStd (Equ x)) = EquStd . Equ $ linExprMapConst f x
ineqStdMapConst f (LteStd (Lte x)) = LteStd . Lte $ linExprMapConst f x
ineqStdMapConst f (GteStd (Gte x)) = GteStd . Gte $ linExprMapConst f x

ineqStdMapVars :: (Map.Map k a -> Map.Map k' a') -> IneqStdForm k a c -> IneqStdForm k' a' c
ineqStdMapVars f (EquStd (Equ x)) = EquStd . Equ $ linExprMapVars f x
ineqStdMapVars f (LteStd (Lte x)) = LteStd . Lte $ linExprMapVars f x
ineqStdMapVars f (GteStd (Gte x)) = GteStd . Gte $ linExprMapVars f x
