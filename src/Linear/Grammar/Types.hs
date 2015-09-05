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

import Prelude hiding (zip, filter)

import Linear.Class
import Data.Set.Class as Sets

import Data.Char
import Data.String
import Data.Key
import Data.Witherable
import qualified Data.Map as Map
import Control.Monad
import Control.Arrow

import Test.QuickCheck
import Test.QuickCheck.Instances ()


-- * Classes

class HasNames a where
  names :: a -> [String]
  mapNames :: (String -> String) -> a -> a

instance HasNames String where
  names x = [x]
  mapNames = ($)

instance (HasNames a, HasNames b, Ord a) => HasNames (Map.Map a b) where
  names xs = concatMap names (Map.keys xs) ++ concatMap names xs
  mapNames f xs = Map.mapKeys (mapNames f) $ mapNames f <$> xs

class HasVariables (a :: * -> *) where
  vars :: a b -> LinVarMap b
  mapVars :: (LinVarMap b -> LinVarMap b0) -> a b -> a b0

class HasCoefficients (a :: * -> *) where
  coeffVals :: a b -> [b]
  mapCoeffVals :: (b -> b0) -> a b -> a b0
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
  arbitrary = sized go
    where
      go :: Int -> Gen LinAst
      go s = oneof
        [ EVar <$> content
        , ELit <$> between1000Rational
        , liftM2 ECoeff (scale (subtract 1) arbitrary) between1000Rational
        , liftM2 EAdd (scale (subtract 1) arbitrary) arbitrary
        ] `suchThat` (\x -> nodeCount x < 1000 && nodeCount x < fromIntegral s)

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

data ErrorSign = ErrNeg | ErrPos
  deriving (Show, Eq, Ord)

instance Arbitrary ErrorSign where
  arbitrary = boolToErrNeg <$> arbitrary

isErrPos :: ErrorSign -> Bool
isErrPos ErrPos = True
isErrPos _ = False

isErrNeg :: ErrorSign -> Bool
isErrNeg = not . isErrPos

boolToErrNeg :: Bool -> ErrorSign
boolToErrNeg b = if b then ErrPos else ErrNeg

data LinVarName =
    VarMain  {unVarMain :: String}
  | VarSlack {unVarSlack :: Integer}
  | VarError {unVarError :: String, unVarErrorSign :: ErrorSign}
  deriving (Show, Eq, Ord)

unLinVarName :: LinVarName -> String
unLinVarName (VarMain n)  = n
unLinVarName (VarSlack n) = show n
unLinVarName (VarError n b) = if isErrPos b then "error_" ++ n ++ "_+"
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

instance Arbitrary LinVar where
  arbitrary = liftM2 LinVar arbitrary between1000Rational

-- | Name-based ordering
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: String -> LinVar -> Bool
hasName n (LinVar m _) = n == unLinVarName m

hasCoeff :: Rational -> LinVar -> Bool
hasCoeff x (LinVar _ y) = x == y

-- * Variables with Coefficients

-- | Mapping from variable names, to a polymorphic coefficient type.
newtype LinVarMap b = LinVarMap
  { unLinVarMap :: Map.Map LinVarName b
  } deriving (Show, Eq, Functor, Foldable, Traversable, Monoid, Lookup)

coeffs :: LinVarMap b -> [b]
coeffs (LinVarMap m) = Map.elems m

mapCoeffs :: (b -> b0) -> LinVarMap b -> LinVarMap b0
mapCoeffs f (LinVarMap m) = LinVarMap $ f <$> m

zipViaCoeffs :: ([b] -> [b]) -> LinVarMap b -> LinVarMap b
zipViaCoeffs f (LinVarMap m) = LinVarMap $ Map.fromList $ uncurry zip $ second f $ unzip $ Map.toList m


instance (CanAddTo b b b, HasZero b, Eq b) => CanAddTo (LinVarMap b) (LinVarMap b) (LinVarMap b) where
  (LinVarMap x) .+. (LinVarMap y) = filter (== zero') $ LinVarMap $ Map.unionWith (.+.) x y

instance (CanSubTo b b b, HasZero b, Eq b) => CanSubTo (LinVarMap b) (LinVarMap b) (LinVarMap b) where
  (LinVarMap x) .-. (LinVarMap y) = filter (== zero') $ LinVarMap $ Map.unionWith (.-.) x y

type instance Key LinVarMap = LinVarName

instance Witherable LinVarMap where
  wither f (LinVarMap xs) = LinVarMap <$> wither f xs

deriving instance HasUnion                   (LinVarMap b)
deriving instance HasIntersection            (LinVarMap b)
deriving instance HasDifference              (LinVarMap b)
deriving instance HasDelete LinVarName       (LinVarMap b)
deriving instance HasInsertWith LinVarName b (LinVarMap b)


instance HasNames (LinVarMap b) where
  names (LinVarMap x) = unLinVarName <$> Map.keys x
  mapNames f (LinVarMap x) = LinVarMap $ Map.mapKeys (mapLinVarName f) x

instance HasVariables LinVarMap where
  vars = id
  mapVars = ($)

instance HasCoefficients LinVarMap where
  coeffVals = coeffs
  mapCoeffVals = mapCoeffs
  zipViaCoeffVals = zipViaCoeffs

instance (Num b, Arbitrary b, Eq b) => Arbitrary (LinVarMap b) where
  arbitrary = LinVarMap <$> arbitrary `suchThat`
    (\x -> Map.size x <= 100 && Map.size x > 0 && notElem 0 x)

-- * Expressions

-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprVars :: LinVarMap Rational
  , exprConst  :: Rational
  } deriving (Show, Eq)

instance HasNames LinExpr where
  names (LinExpr xs _) = names xs
  mapNames f (LinExpr xs xc) = LinExpr (mapNames f xs) xc

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

instance Arbitrary IneqExpr where
  arbitrary = oneof
    [ liftM2 EquExpr arbitrary arbitrary
    , liftM2 LteExpr arbitrary arbitrary
    ]

-- * Standard Form Equations

data Equality b = Equ (LinVarMap b) Rational
  deriving (Show, Eq, Functor, Foldable, Traversable) -- TODO: Make Lookup instance!

instance HasNames (Equality b) where
  names (Equ xs _) = names xs
  mapNames f (Equ xs xc) = Equ (mapNames f xs) xc

instance HasVariables Equality where
  vars (Equ xs _) = xs
  mapVars f (Equ xs xc) = Equ (mapVars f xs) xc

instance HasCoefficients Equality where
  coeffVals (Equ xs _) = coeffVals xs
  mapCoeffVals f (Equ xs xc) = Equ (mapCoeffVals f xs) xc
  zipViaCoeffVals f (Equ xs xc) = Equ (zipViaCoeffVals f xs) xc

instance HasConstant (Equality b) where
  constVal (Equ _ xc) = xc
  mapConst f (Equ xs xc) = Equ xs $ f xc

instance (Num b, Eq b, Arbitrary b) => Arbitrary (Equality b) where
  arbitrary = liftM2 Equ arbitrary between1000Rational

data LInequality b = Lte (LinVarMap b) Rational
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance HasNames (LInequality b) where
  names (Lte xs _) = names xs
  mapNames f (Lte xs xc) = Lte (mapNames f xs) xc

instance HasVariables LInequality where
  vars (Lte xs _) = xs
  mapVars f (Lte xs xc) = Lte (mapVars f xs) xc

instance HasCoefficients LInequality where
  coeffVals (Lte xs _) = coeffVals xs
  mapCoeffVals f (Lte xs xc) = Lte (mapCoeffVals f xs) xc
  zipViaCoeffVals f (Lte xs xc) = Lte (zipViaCoeffVals f xs) xc

instance HasConstant (LInequality b) where
  constVal (Lte _ xc) = xc
  mapConst f (Lte xs xc) = Lte xs $ f xc

instance (Num b, Eq b, Arbitrary b) => Arbitrary (LInequality b) where
  arbitrary = liftM2 Lte arbitrary between1000Rational

data GInequality b = Gte (LinVarMap b) Rational
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance HasNames (GInequality b) where
  names (Gte xs _) = names xs
  mapNames f (Gte xs xc) = Gte (mapNames f xs) xc

instance HasVariables GInequality where
  vars (Gte xs _) = xs
  mapVars f (Gte xs xc) = Gte (mapVars f xs) xc

instance HasCoefficients GInequality where
  coeffVals (Gte xs _) = coeffVals xs
  mapCoeffVals f (Gte xs xc) = Gte (mapCoeffVals f xs) xc
  zipViaCoeffVals f (Gte xs xc) = Gte (zipViaCoeffVals f xs) xc

instance HasConstant (GInequality b) where
  constVal (Gte _ xc) = xc
  mapConst f (Gte xs xc) = Gte xs $ f xc

instance (Num b, Eq b, Arbitrary b) => Arbitrary (GInequality b) where
  arbitrary = liftM2 Gte arbitrary between1000Rational

-- | Internal structure for linear equations
data IneqStdForm b =
    EquStd {unEquStd :: Equality b}
  | LteStd {unLteStd :: LInequality b}
  | GteStd {unGteStd :: GInequality b}
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance HasNames (IneqStdForm b) where
  names (EquStd x) = names x
  names (LteStd x) = names x
  names (GteStd x) = names x
  mapNames f (EquStd x) = EquStd $ mapNames f x
  mapNames f (LteStd x) = LteStd $ mapNames f x
  mapNames f (GteStd x) = GteStd $ mapNames f x

instance HasVariables IneqStdForm where
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

instance (Num b, Eq b, Arbitrary b) => Arbitrary (IneqStdForm b) where
  arbitrary = oneof
    [ EquStd <$> arbitrary
    , LteStd <$> arbitrary
    , GteStd <$> arbitrary
    ]


between1000Rational :: Gen Rational
between1000Rational = fromIntegral <$> (arbitrary :: Gen Int)
  `suchThat` (\x -> x <= 1000 && x >= -1000)
