{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , KindSignatures
  #-}

module Linear.Grammar.Types where

import Linear.Class
import Sets.Class

import Data.Char
import Data.String
import Data.Monoid
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative
import Control.Arrow

import Test.QuickCheck
import Test.QuickCheck.Instances ()


-- * Classes

class HasNames a where
  names :: a -> [String]
  mapNames :: (String -> String) -> a -> a

class HasVariables a b where
  vars :: a -> LinVarMap b
  mapVars :: (LinVarMap b -> LinVarMap b) -> a -> a

class HasCoefficients (a :: * -> *) where
  coeffVals :: a b -> [b]
  mapCoeffVals :: (b -> b) -> a b -> a b
  zipViaCoeffVals :: ([b] -> [b]) -> a b -> a b

class HasConstant a where
  constVal :: a -> Rational
  mapConst :: (Rational -> Rational) -> a -> a

-- * User-facing API

-- | User-facing abstract syntax tree
data LinAst =
    EVar String
  | ELit Rational
  | ECoeff LinAst Rational
  | EAdd LinAst LinAst
  deriving (Show, Eq)

instance HasNames LinAst where
  names (EVar n) = [n]
  names (ELit _) = []
  names (ECoeff e _) = names e
  names (EAdd e1 e2) = names e1 ++ names e2
  mapNames f (EVar n) = EVar $ f n
  mapNames _ (ELit x) = ELit x
  mapNames f (ECoeff e c) = ECoeff (mapNames f e) c
  mapNames f (EAdd e1 e2) = EAdd (mapNames f e1) (mapNames f e2)

instance Arbitrary LinAst where
  arbitrary = oneof
    [ EVar <$> content
    , ELit <$> between1000Rational
    , liftM2 ECoeff arbitrary between1000Rational
    , liftM2 EAdd arbitrary arbitrary
    ] `suchThat` (\x -> nodeCount x < 1000)
    where
      nodeCount :: LinAst -> Integer
      nodeCount (EVar _) = 1
      nodeCount (ELit _) = 1
      nodeCount (ECoeff e _) = 2 + nodeCount e
      nodeCount (EAdd e1 e2) = nodeCount e1 + nodeCount e2 + 1

      content = arbitrary `suchThat` (\x -> length x < 5
                                         && not (null x)
                                         && all isAlpha x)

instance IsString LinAst where
  fromString = EVar

instance CanAddTo LinAst LinAst LinAst where
  (.+.) = EAdd

instance CanMultiplyTo LinAst Rational LinAst where
  (.*.) = ECoeff

instance CanMultiplyTo Rational LinAst LinAst where
  (.*.) = flip ECoeff


-- * Variables

data LinVarName =
    VarMain  {unVarMain :: String}
  | VarSlack {unVarSlack :: Integer}
  | VarError {unVarError :: String, unVarErrorSign :: Bool}
  deriving (Show, Eq, Ord)

unLinVarName :: LinVarName -> String
unLinVarName (VarMain n)  = n
unLinVarName (VarSlack n) = show n
unLinVarName (VarError n b) = if b then "error_" ++ n ++ "_+"
                                   else "error_" ++ n ++ "_-"

mapLinVarName :: (String -> String) -> LinVarName -> LinVarName
mapLinVarName f (VarMain n)  = VarMain $ f n
mapLinVarName f (VarError n b) = VarError (f n) b
mapLinVarName _ n = n

instance Arbitrary LinVarName where
  arbitrary = oneof [ VarMain  <$> content
                    , VarSlack <$> arbitrary `suchThat` (\x -> x <= 1000 && x >= 0)
                    , liftM2 VarError content arbitrary
                    ]
    where
      content = arbitrary `suchThat` (\x -> length x < 5
                                         && not (null x)
                                         && all isAlpha x)


data LinVar = LinVar
  { varName  :: LinVarName
  , varCoeff :: Rational
  } deriving (Show, Eq)

instance HasNames LinVar where
  names (LinVar n _) = [unLinVarName n]
  mapNames f (LinVar n c) = LinVar (mapLinVarName f n) c

instance HasVariables LinVar Rational where
  vars (LinVar n c) = LinVarMap $ Map.singleton n c
  mapVars f x = uncurry LinVar $ head $ Map.toList $ unLinVarMap $ f $ vars x

instance Arbitrary LinVar where
  arbitrary = liftM2 LinVar arbitrary between1000Rational

-- | Name-based ordering
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: String -> LinVar -> Bool
hasName n (LinVar m _) = n == unLinVarName m

hasCoeff :: Rational -> LinVar -> Bool
hasCoeff x (LinVar _ y) = x == y

-- | Variables with coefficients
newtype LinVarMap b = LinVarMap
  { unLinVarMap :: Map.Map LinVarName b
  } deriving (Show, Eq)

coeffs :: LinVarMap b -> [b]
coeffs (LinVarMap m) = Map.elems m

mapCoeffs :: (b -> b) -> LinVarMap b -> LinVarMap b
mapCoeffs f (LinVarMap m) = LinVarMap $ f <$> m

zipViaCoeffs :: ([b] -> [b]) -> LinVarMap b -> LinVarMap b
zipViaCoeffs f (LinVarMap m) = LinVarMap $ Map.fromList $ uncurry zip $ second f $ unzip $ Map.toList m

deriving instance Monoid (LinVarMap b)
deriving instance HasUnion (LinVarMap b)
deriving instance HasIntersection (LinVarMap b)
deriving instance HasDifference (LinVarMap b)

instance HasNames (LinVarMap b) where
  names (LinVarMap x) = unLinVarName <$> Map.keys x
  mapNames f (LinVarMap x) = LinVarMap $ Map.mapKeys (mapLinVarName f) x

instance HasVariables (LinVarMap b) b where
  vars = id
  mapVars = ($)

instance HasCoefficients LinVarMap where
  coeffVals = coeffs
  mapCoeffVals = mapCoeffs
  zipViaCoeffVals = zipViaCoeffs

instance Arbitrary b => Arbitrary (LinVarMap b) where
  arbitrary = LinVarMap <$> arbitrary `suchThat`
    (\x -> Map.size x <= 100 && Map.size x > 0)

-- * Expressions

-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprVars :: LinVarMap Rational
  , exprConst  :: Rational
  } deriving (Show, Eq)

instance HasNames LinExpr where
  names (LinExpr xs _) = names xs
  mapNames f (LinExpr xs xc) = LinExpr (mapNames f xs) xc

instance HasVariables LinExpr Rational where
  vars (LinExpr xs _) = xs
  mapVars f (LinExpr xs xc) = LinExpr (f xs) xc

instance HasConstant LinExpr where
  constVal (LinExpr _ xc) = xc
  mapConst f (LinExpr xs xc) = LinExpr xs $ f xc

instance Arbitrary LinExpr where
  arbitrary = liftM2 LinExpr arbitrary between1000Rational

mergeLinExpr :: LinExpr -> LinExpr -> LinExpr
mergeLinExpr (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (vs1 `union` vs2) (x + y)


-- * Equations

-- | User-level inequalities
data IneqExpr =
    EquExpr LinExpr LinExpr
  | LteExpr LinExpr LinExpr
  deriving (Show, Eq)

instance HasNames IneqExpr where
  names (EquExpr x y) = names x ++ names y
  names (LteExpr x y) = names x ++ names y
  mapNames f (EquExpr x y) = EquExpr (mapNames f x) (mapNames f y)
  mapNames f (LteExpr x y) = LteExpr (mapNames f x) (mapNames f y)

instance HasVariables IneqExpr Rational where
  vars (EquExpr x y) = vars x `union` vars y
  vars (LteExpr x y) = vars x `union` vars y
  mapVars f (EquExpr x y) = EquExpr (mapVars f x) (mapVars f y)
  mapVars f (LteExpr x y) = LteExpr (mapVars f x) (mapVars f y)

instance Arbitrary IneqExpr where
  arbitrary = oneof
    [ liftM2 EquExpr arbitrary arbitrary
    , liftM2 LteExpr arbitrary arbitrary
    ]

-- * Standard Form Equations

data Equality b = Equ (LinVarMap b) Rational
  deriving (Show, Eq)

instance HasNames (Equality b) where
  names (Equ xs _) = names xs
  mapNames f (Equ xs xc) = Equ (mapNames f xs) xc

instance HasVariables (Equality b) b where
  vars (Equ xs _) = xs
  mapVars f (Equ xs xc) = Equ (mapVars f xs) xc

instance HasCoefficients Equality where
  coeffVals (Equ xs _) = coeffVals xs
  mapCoeffVals f (Equ xs xc) = Equ (mapCoeffVals f xs) xc
  zipViaCoeffVals f (Equ xs xc) = Equ (zipViaCoeffVals f xs) xc

instance HasConstant (Equality b) where
  constVal (Equ _ xc) = xc
  mapConst f (Equ xs xc) = Equ xs $ f xc

instance Arbitrary b => Arbitrary (Equality b) where
  arbitrary = liftM2 Equ arbitrary arbitrary

data LInequality b = Lte (LinVarMap b) Rational
  deriving (Show, Eq)

instance HasNames (LInequality b) where
  names (Lte xs _) = names xs
  mapNames f (Lte xs xc) = Lte (mapNames f xs) xc

instance HasVariables (LInequality b) b where
  vars (Lte xs _) = xs
  mapVars f (Lte xs xc) = Lte (mapVars f xs) xc

instance HasCoefficients LInequality where
  coeffVals (Lte xs _) = coeffVals xs
  mapCoeffVals f (Lte xs xc) = Lte (mapCoeffVals f xs) xc
  zipViaCoeffVals f (Lte xs xc) = Lte (zipViaCoeffVals f xs) xc

instance HasConstant (LInequality b) where
  constVal (Lte _ xc) = xc
  mapConst f (Lte xs xc) = Lte xs $ f xc

instance Arbitrary b => Arbitrary (LInequality b) where
  arbitrary = liftM2 Lte arbitrary arbitrary

data GInequality b = Gte (LinVarMap b) Rational
  deriving (Show, Eq)

instance HasNames (GInequality b) where
  names (Gte xs _) = names xs
  mapNames f (Gte xs xc) = Gte (mapNames f xs) xc

instance HasVariables (GInequality b) b where
  vars (Gte xs _) = xs
  mapVars f (Gte xs xc) = Gte (mapVars f xs) xc

instance HasCoefficients GInequality where
  coeffVals (Gte xs _) = coeffVals xs
  mapCoeffVals f (Gte xs xc) = Gte (mapCoeffVals f xs) xc
  zipViaCoeffVals f (Gte xs xc) = Gte (zipViaCoeffVals f xs) xc

instance HasConstant (GInequality b) where
  constVal (Gte _ xc) = xc
  mapConst f (Gte xs xc) = Gte xs $ f xc

instance Arbitrary b => Arbitrary (GInequality b) where
  arbitrary = liftM2 Gte arbitrary arbitrary

-- | Internal structure for linear equations
data IneqStdForm b =
    EquStd {unEquStd :: Equality b}
  | LteStd {unLteStd :: LInequality b}
  | GteStd {unGteStd :: GInequality b}
  deriving (Show, Eq)

instance HasNames (IneqStdForm b) where
  names (EquStd x) = names x
  names (LteStd x) = names x
  names (GteStd x) = names x
  mapNames f (EquStd x) = EquStd $ mapNames f x
  mapNames f (LteStd x) = LteStd $ mapNames f x
  mapNames f (GteStd x) = GteStd $ mapNames f x

instance HasVariables (IneqStdForm b) b where
  vars (EquStd x) = vars x
  vars (LteStd x) = vars x
  vars (GteStd x) = vars x
  mapVars f (EquStd x) = EquStd $ mapVars f x
  mapVars f (LteStd x) = LteStd $ mapVars f x
  mapVars f (GteStd x) = GteStd $ mapVars f x

instance HasCoefficients IneqStdForm where
  coeffVals (EquStd x) = coeffVals x
  coeffVals (LteStd x) = coeffVals x
  coeffVals (GteStd x) = coeffVals x
  mapCoeffVals f (EquStd x) = EquStd $ mapCoeffVals f x
  mapCoeffVals f (LteStd x) = LteStd $ mapCoeffVals f x
  mapCoeffVals f (GteStd x) = GteStd $ mapCoeffVals f x
  zipViaCoeffVals f (EquStd x) = EquStd $ zipViaCoeffVals f x
  zipViaCoeffVals f (LteStd x) = LteStd $ zipViaCoeffVals f x
  zipViaCoeffVals f (GteStd x) = GteStd $ zipViaCoeffVals f x

instance HasConstant (IneqStdForm b) where
  constVal (EquStd x) = constVal x
  constVal (LteStd x) = constVal x
  constVal (GteStd x) = constVal x
  mapConst f (EquStd x) = EquStd $ mapConst f x
  mapConst f (LteStd x) = LteStd $ mapConst f x
  mapConst f (GteStd x) = GteStd $ mapConst f x

instance Arbitrary b => Arbitrary (IneqStdForm b) where
  arbitrary = oneof
    [ EquStd <$> arbitrary
    , LteStd <$> arbitrary
    , GteStd <$> arbitrary
    ]


between1000Rational :: Gen Rational
between1000Rational = fromIntegral <$> (arbitrary :: Gen Int)
  `suchThat` (\x -> x <= 1000 && x >= -1000)
