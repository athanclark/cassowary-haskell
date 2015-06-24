{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Linear.Grammar.Types where

import Linear.Grammar.Class
import Sets.Class

import Data.Char
import Data.String
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

instance HasVariables LinAst [String] where
  names (EVar n) = [n]
  names (ELit _) = []
  names (ECoeff e _) = names e
  names (EAdd e1 e2) = names e1 ++ names e2
  mapNames f (EVar n) = EVar $ f n
  mapNames _ (ELit x) = ELit x
  mapNames f (ECoeff e c) = ECoeff (mapNames f e) c
  mapNames f (EAdd e1 e2) = EAdd (mapNames f e1) (mapNames f e2)
  vars (EVar n) = [n]
  vars (ELit _) = []
  vars (ECoeff e _) = names e
  vars (EAdd e1 e2) = names e1 ++ names e2
  mapVars f (EVar n) = EVar $ head $ f [n]
  mapVars _ (ELit x) = ELit x
  mapVars f (ECoeff e c) = ECoeff (mapVars f e) c
  mapVars f (EAdd e1 e2) = EAdd (mapVars f e1) (mapVars f e2)

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

(.+.) :: LinAst -> LinAst -> LinAst
(.+.) = EAdd

infixr 8 .+.

class Coefficient x y where
  (.*.) :: x -> y -> LinAst

infixr 9 .*.

instance Coefficient LinAst Rational where
  (.*.) = ECoeff

instance Coefficient Rational LinAst where
  (.*.) = flip ECoeff


-- * Variables

data LinVarName =
    VarMain  String
  | VarSlack Integer
  | VarError String Bool
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

instance HasVariables LinVar LinVarMap where
  names (LinVar n _) = [unLinVarName n]
  mapNames f (LinVar n c) = LinVar (mapLinVarName f n) c
  vars (LinVar n c) = LinVarMap $ Map.singleton n c
  mapVars f x = uncurry LinVar $ head $ Map.toList $ unLinVarMap $ f $ vars x

instance HasCoefficients LinVar where
  coeffVals (LinVar _ x) = [x]
  mapCoeffs f (LinVar n x) = LinVar n $ head $ f [x]

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
newtype LinVarMap = LinVarMap
  { unLinVarMap :: Map.Map LinVarName Rational
  } deriving (Show, Eq)

deriving instance Monoid LinVarMap
deriving instance HasUnion LinVarMap
deriving instance HasIntersection LinVarMap
deriving instance HasDifference LinVarMap

instance HasVariables LinVarMap LinVarMap where
  names (LinVarMap x) = unLinVarName <$> Map.keys x
  mapNames f (LinVarMap x) = LinVarMap $ Map.mapKeys (mapLinVarName f) x
  vars = id
  mapVars = ($)

instance HasCoefficients LinVarMap where
  coeffVals (LinVarMap xs) = snd $ unzip $ Map.toList xs
  mapCoeffs f (LinVarMap xs) =
    let (ks,vs) = unzip $ Map.toList xs
    in LinVarMap $ Map.fromList $ zip ks $ f vs

instance Arbitrary LinVarMap where
  arbitrary = LinVarMap <$> arbitrary `suchThat`
    (\x -> Map.size x <= 100 && Map.size x > 0)

-- * Expressions

-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprVars :: LinVarMap
  , exprConst  :: Rational
  } deriving (Show, Eq)

instance HasVariables LinExpr LinVarMap where
  names (LinExpr xs _) = names xs
  mapNames f (LinExpr xs xc) = LinExpr (mapNames f xs) xc
  vars (LinExpr xs _) = xs
  mapVars f (LinExpr xs xc) = LinExpr (f xs) xc

instance HasCoefficients LinExpr where
  coeffVals (LinExpr xs _) = coeffVals xs
  mapCoeffs f (LinExpr xs xc) = LinExpr (mapCoeffs f xs) xc

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

instance HasVariables IneqExpr LinVarMap where
  names (EquExpr x y) = names x ++ names y
  names (LteExpr x y) = names x ++ names y
  mapNames f (EquExpr x y) = EquExpr (mapNames f x) (mapNames f y)
  mapNames f (LteExpr x y) = LteExpr (mapNames f x) (mapNames f y)
  vars (EquExpr x y) = vars x `union` vars y
  vars (LteExpr x y) = vars x `union` vars y
  mapVars f (EquExpr x y) = EquExpr (mapVars f x) (mapVars f y)
  mapVars f (LteExpr x y) = LteExpr (mapVars f x) (mapVars f y)

instance HasCoefficients IneqExpr where
  coeffVals (EquExpr x y) = coeffVals x ++ coeffVals y
  coeffVals (LteExpr x y) = coeffVals x ++ coeffVals y
  mapCoeffs f (EquExpr x y) = EquExpr (mapCoeffs f x) (mapCoeffs f y)
  mapCoeffs f (LteExpr x y) = LteExpr (mapCoeffs f x) (mapCoeffs f y)

instance Arbitrary IneqExpr where
  arbitrary = oneof
    [ liftM2 EquExpr arbitrary arbitrary
    , liftM2 LteExpr arbitrary arbitrary
    ]

-- * Standard Form Equations

data Equality = Equ LinVarMap Rational
  deriving (Show, Eq)

instance HasVariables Equality LinVarMap where
  names (Equ xs _) = names xs
  mapNames f (Equ xs xc) = Equ (mapNames f xs) xc
  vars (Equ xs _) = xs
  mapVars f (Equ xs xc) = Equ (mapVars f xs) xc

instance HasCoefficients Equality where
  coeffVals (Equ xs _) = coeffVals xs
  mapCoeffs f (Equ xs xc) = Equ (mapCoeffs f xs) xc

instance HasConstant Equality where
  constVal (Equ _ xc) = xc
  mapConst f (Equ xs xc) = Equ xs $ f xc

instance Arbitrary Equality where
  arbitrary = liftM2 Equ arbitrary between1000Rational

data LInequality = Lte LinVarMap Rational
  deriving (Show, Eq)

instance HasVariables LInequality LinVarMap where
  names (Lte xs _) = names xs
  mapNames f (Lte xs xc) = Lte (mapNames f xs) xc
  vars (Lte xs _) = xs
  mapVars f (Lte xs xc) = Lte (mapVars f xs) xc

instance HasCoefficients LInequality where
  coeffVals (Lte xs _) = coeffVals xs
  mapCoeffs f (Lte xs xc) = Lte (mapCoeffs f xs) xc

instance HasConstant LInequality where
  constVal (Lte _ xc) = xc
  mapConst f (Lte xs xc) = Lte xs $ f xc

instance Arbitrary LInequality where
  arbitrary = liftM2 Lte arbitrary between1000Rational

data GInequality = Gte LinVarMap Rational
  deriving (Show, Eq)

instance HasVariables GInequality LinVarMap where
  names (Gte xs _) = names xs
  mapNames f (Gte xs xc) = Gte (mapNames f xs) xc
  vars (Gte xs _) = xs
  mapVars f (Gte xs xc) = Gte (mapVars f xs) xc

instance HasCoefficients GInequality where
  coeffVals (Gte xs _) = coeffVals xs
  mapCoeffs f (Gte xs xc) = Gte (mapCoeffs f xs) xc

instance HasConstant GInequality where
  constVal (Gte _ xc) = xc
  mapConst f (Gte xs xc) = Gte xs $ f xc

instance Arbitrary GInequality where
  arbitrary = liftM2 Gte arbitrary between1000Rational

-- | Internal structure for linear equations
data IneqStdForm =
    EquStd {unEquStd :: Equality}
  | LteStd {unLteStd :: LInequality}
  | GteStd {unGteStd :: GInequality}
  deriving (Show, Eq)

instance HasVariables IneqStdForm LinVarMap where
  names (EquStd x) = names x
  names (LteStd x) = names x
  names (GteStd x) = names x
  mapNames f (EquStd x) = EquStd $ mapNames f x
  mapNames f (LteStd x) = LteStd $ mapNames f x
  mapNames f (GteStd x) = GteStd $ mapNames f x
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
  mapCoeffs f (EquStd x) = EquStd $ mapCoeffs f x
  mapCoeffs f (LteStd x) = LteStd $ mapCoeffs f x
  mapCoeffs f (GteStd x) = GteStd $ mapCoeffs f x

instance HasConstant IneqStdForm where
  constVal (EquStd x) = constVal x
  constVal (LteStd x) = constVal x
  constVal (GteStd x) = constVal x
  mapConst f (EquStd x) = EquStd $ mapConst f x
  mapConst f (LteStd x) = LteStd $ mapConst f x
  mapConst f (GteStd x) = GteStd $ mapConst f x

instance Arbitrary IneqStdForm where
  arbitrary = oneof
    [ EquStd <$> arbitrary
    , LteStd <$> arbitrary
    , GteStd <$> arbitrary
    ]


between1000Rational :: Gen Rational
between1000Rational = fromIntegral <$> (arbitrary :: Gen Int)
  `suchThat` (\x -> x <= 1000 && x >= -1000)
