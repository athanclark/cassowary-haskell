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

import Data.Char
import Data.String
import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad

import Test.QuickCheck
import Test.QuickCheck.Instances ()


-- * User-facing API

-- | User-facing abstract syntax tree
data LinAst =
    EVar String
  | ELit Rational
  | ECoeff LinAst Rational
  | EAdd LinAst LinAst
  deriving (Show, Eq)


instance Arbitrary LinAst where
  arbitrary = sized go
    where
      go :: Int -> Gen LinAst
      go s | s <= 1 = oneof [ EVar <$> stringName
                            , ELit <$> between1000Rational
                            ]
           | otherwise = oneof [ EVar <$> stringName
                               , ELit <$> between1000Rational
                               , liftM2 ECoeff (scale (subtract 1) arbitrary)
                                          between1000Rational
                               , do n <- choose (0,s-1)
                                    liftM2 EAdd (resize n arbitrary)
                                           (resize n arbitrary)
                               ]


instance IsString LinAst where
  fromString = EVar

(.+.) :: LinAst -> LinAst -> LinAst
(.+.) = EAdd

(.*.) :: LinAst -> Rational -> LinAst
(.*.) = ECoeff


-- * Variables

data ErrorSign = ErrNeg | ErrPos
  deriving (Show, Eq, Ord)

instance Arbitrary ErrorSign where
  arbitrary = boolToErrorSign <$> arbitrary

isErrPos :: ErrorSign -> Bool
isErrPos ErrPos = True
isErrPos _      = False

isErrNeg :: ErrorSign -> Bool
isErrNeg = not . isErrPos

boolToErrorSign :: Bool -> ErrorSign
boolToErrorSign b = if b then ErrPos else ErrNeg

-- | /Restricted/ linear variable name types - only slack variables and error
-- variables are known to be @>= 0).
data RLinVarName =
    VarSlack {unVarSlack :: Int}
  | VarError {unVarError :: String, unVarErrorSign :: ErrorSign}
  deriving (Show, Eq, Ord)

instance Arbitrary RLinVarName where
  arbitrary = oneof [ VarSlack <$> arbitrary `suchThat` (\x -> x <= 1000 && x >= 0)
                    , liftM2 VarError stringName arbitrary
                    ]

-- | Includes both restricted and unrestricted variables
data LinVarName =
    VarMain       {unVarMain       :: String}
  | VarRestricted {unVarRestricted :: RLinVarName}
  deriving (Show, Eq, Ord)

unLinVarName :: LinVarName -> String
unLinVarName (VarMain n)    = n
unLinVarName (VarRestricted (VarSlack n))   = show n
unLinVarName (VarRestricted (VarError n b)) = "error_" ++ n ++ if isErrPos b
                                                               then "_+"
                                                               else "_-"

mapLinVarName :: (String -> String) -> LinVarName -> LinVarName
mapLinVarName f (VarMain n)    = VarMain $ f n
mapLinVarName f (VarRestricted (VarError n b)) = VarRestricted $ VarError (f n) b
mapLinVarName _ n              = n

instance Arbitrary LinVarName where
  arbitrary = oneof [ VarMain       <$> stringName
                    , VarRestricted <$> arbitrary
                    ]


data LinVar = LinVar
  { varName  :: LinVarName
  , varCoeff :: Rational
  } deriving (Show, Eq)

instance Arbitrary LinVar where
  arbitrary = liftM2 LinVar arbitrary between1000Rational

-- | Name-based ordering
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

linVarHasName :: String -> LinVar -> Bool
linVarHasName n (LinVar m _) = n == unLinVarName m

linVarHasCoeff :: Rational -> LinVar -> Bool
linVarHasCoeff x (LinVar _ y) = x == y

-- * Variables with Coefficients


addMap :: (Ord k, Eq a, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
addMap xs ys = Map.filter (not . (== 0)) $ Map.unionWith (+) xs ys

subMap :: (Ord k, Eq a, Num a) => Map.Map k a -> Map.Map k a -> Map.Map k a
subMap xs ys = addMap xs (negate <$> ys)

-- * Expressions

-- | Linear expressions suited for normal and standard form.
data LinExpr var coeff = LinExpr
  { linExprVars  :: Map.Map var coeff
  , linExprConst :: Rational
  } deriving (Show, Eq, Functor, Foldable, Traversable)


instance (Ord k, Eq a, Arbitrary k, Arbitrary a, Num a) => Arbitrary (LinExpr k a) where
  arbitrary = do
    xs <- arbitrary `suchThat` (\xs -> Map.size xs <= 1000
                                    && Map.size xs > 0
                                    && not (any (== 0) xs))
    (LinExpr xs) <$> between1000Rational

mergeLinExpr :: (Ord k, Eq a, Num a) => LinExpr k a -> LinExpr k a -> LinExpr k a
mergeLinExpr (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (addMap vs1 vs2) (x + y)

instance (Ord k, Eq a, Num a) => Semigroup (LinExpr k a) where
  (<>) = mergeLinExpr

instance (Ord k, Eq a, Num a) => Monoid (LinExpr k a) where
  mempty = LinExpr mempty 0
  mappend = mergeLinExpr

linExprKeys :: LinExpr k a -> [k]
linExprKeys (LinExpr xs _) = Map.keys xs

linExprKeysSet :: LinExpr k a -> Set.Set k
linExprKeysSet (LinExpr xs _) = Map.keysSet xs

linExprMapConst :: (Rational -> Rational) -> LinExpr k a -> LinExpr k a
linExprMapConst f (LinExpr xs xc) = LinExpr xs (f xc)

linExprMapVars :: (Map.Map k0 a -> Map.Map k1 b) -> LinExpr k0 a -> LinExpr k1 b
linExprMapVars f (LinExpr xs xc) = LinExpr (f xs) xc


-- * Equations

-- | User-level inequalities
data IneqExpr k a =
    EquExpr (LinExpr k a) (LinExpr k a)
  | LteExpr (LinExpr k a) (LinExpr k a)
  deriving (Show, Eq)

instance (Ord k, Arbitrary k, Arbitrary a, Num a, Eq a) => Arbitrary (IneqExpr k a) where
  arbitrary = oneof
    [ liftM2 EquExpr arbitrary arbitrary
    , liftM2 LteExpr arbitrary arbitrary
    ]

-- * Standard Form Equations

-- Exact equality
newtype Equality k a = Equ {getEqu :: LinExpr k a}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- Less-than inequality
newtype LInequality k a = Lte {getLte :: LinExpr k a}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- Greater-than inequality
newtype GInequality k a = Gte {getGte :: LinExpr k a}
  deriving (Show, Eq, Functor, Foldable, Traversable, Arbitrary)

-- | Internal structure for linear equations
data IneqStdForm k a =
    EquStd {unEquStd :: Equality    k a}
  | LteStd {unLteStd :: LInequality k a}
  | GteStd {unGteStd :: GInequality k a}
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (Ord k, Arbitrary k, Arbitrary a, Num a, Eq a) => Arbitrary (IneqStdForm k a) where
  arbitrary = oneof [ EquStd <$> arbitrary
                    , LteStd <$> arbitrary
                    , GteStd <$> arbitrary
                    ]


-- | Lose inequality data
ineqStdToLinExpr :: IneqStdForm k a -> LinExpr k a
ineqStdToLinExpr (EquStd (Equ x)) = x
ineqStdToLinExpr (LteStd (Lte x)) = x
ineqStdToLinExpr (GteStd (Gte x)) = x

ineqStdKeys :: IneqStdForm k a -> [k]
ineqStdKeys = linExprKeys . ineqStdToLinExpr

ineqStdKeysSet :: IneqStdForm k a -> Set.Set k
ineqStdKeysSet = linExprKeysSet . ineqStdToLinExpr

ineqStdConst :: IneqStdForm k a -> Rational
ineqStdConst = linExprConst . ineqStdToLinExpr

ineqStdVars :: IneqStdForm k a -> Map.Map k a
ineqStdVars = linExprVars . ineqStdToLinExpr

ineqStdMapConst :: (Rational -> Rational) -> IneqStdForm k a -> IneqStdForm k a
ineqStdMapConst f (EquStd (Equ x)) = EquStd . Equ $ linExprMapConst f x
ineqStdMapConst f (LteStd (Lte x)) = LteStd . Lte $ linExprMapConst f x
ineqStdMapConst f (GteStd (Gte x)) = GteStd . Gte $ linExprMapConst f x

ineqStdMapVars :: (Map.Map k0 a -> Map.Map k1 b) -> IneqStdForm k0 a -> IneqStdForm k1 b
ineqStdMapVars f (EquStd (Equ x)) = EquStd . Equ $ linExprMapVars f x
ineqStdMapVars f (LteStd (Lte x)) = LteStd . Lte $ linExprMapVars f x
ineqStdMapVars f (GteStd (Gte x)) = GteStd . Gte $ linExprMapVars f x



-- * Orphan instances

between1000Rational :: Gen Rational
between1000Rational = fromIntegral <$> (arbitrary :: Gen Int)
  `suchThat` (\x -> x <= 1000 && x >= -1000)


stringName :: Gen String
stringName = arbitrary `suchThat`
  (\x -> length x < 5 && not (null x) && all isAlpha x)
