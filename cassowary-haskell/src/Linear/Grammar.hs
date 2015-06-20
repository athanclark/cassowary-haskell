{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , TypeSynonymInstances
  , FlexibleInstances
  , StandaloneDeriving
  , GeneralizedNewtypeDeriving
  #-}

module Linear.Grammar where

import Linear.Grammar.Class
import Sets.Class

import Data.Char
import Data.List hiding (union)
import Data.String
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Control.Applicative

import Test.QuickCheck


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
    [ EVar <$> (:[]) <$> choose ('A','z')
    , ELit <$> between1000Rational
    , liftM2 ECoeff arbitrary between1000Rational
    , liftM2 EAdd arbitrary arbitrary
    ]

-- | Doesn't solve the ridde, but it helps.
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

-- | Pushes @ECoeff@ down the tree, leaving @EAdd@ at the top level.
-- After using this funciton, all @ECoeff@ constructors @LinAst@ parameter will
-- be @EVar@.
multLin :: LinAst -> LinAst
multLin (EVar n) = EVar n
multLin (ELit x) = ELit x
multLin (ECoeff e x) = case multLin e of
  (ELit y)      -> ELit (y * x)
  (EVar n)      -> ECoeff (EVar n) x
  (ECoeff e' y) -> ECoeff e' (y * x)
  (EAdd e1 e2)  -> EAdd (multLin $ ECoeff e1 x) (multLin $ ECoeff e2 x)
multLin (EAdd e1 e2) = EAdd (multLin e1) (multLin e2)

-- * Linear Expressions

data LinVarName =
    VarMain  String
  | VarSlack Integer
  | VarError String Bool
  deriving (Show, Eq, Ord)

instance Arbitrary LinVarName where
  arbitrary = oneof [ VarMain  <$> content
                    , VarSlack <$> arbitrary
                    , liftM2 VarError content arbitrary
                    ]
    where
      content = arbitrary `suchThat` (\x -> length x < 5
                                         && not (null x)
                                         && all isAlpha x)

unLinVarName :: LinVarName -> String
unLinVarName (VarMain n)  = n
unLinVarName (VarSlack n) = show n
unLinVarName (VarError n b) = if b then "error_" ++ n ++ "_+"
                                   else "error_" ++ n ++ "_-"

mapLinVarName :: (String -> String) -> LinVarName -> LinVarName
mapLinVarName f (VarMain n)  = VarMain $ f n
mapLinVarName f (VarError n b) = VarError (f n) b
mapLinVarName _ n = n


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
  arbitrary = liftM2 LinVar arbitrary
                            between1000Rational

-- | Name-based ordering
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: String -> LinVar -> Bool
hasName n (LinVar m _) = n == unLinVarName m

hasCoeff :: Rational -> LinVar -> Bool
hasCoeff x (LinVar _ y) = x == y


instance Arbitrary Rational where
  arbitrary = between1000Rational


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
  arbitrary = (LinVarMap . Map.fromList) <$> arbitrary


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
  arbitrary = liftM2 LinExpr (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed = hasNoDups . names

mergeLinExpr :: LinExpr -> LinExpr -> LinExpr
mergeLinExpr (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (vs1 `union` vs2) (x + y)

-- | Turns @LinAst@ to @LinExpr@ - should be done /after/ @multLin@.
addLin :: LinAst -> LinExpr
addLin = go (LinExpr (LinVarMap Map.empty) 0)
  where
    go :: LinExpr -> LinAst -> LinExpr
    go (LinExpr (LinVarMap vs) c) (EVar n) =
      LinExpr (LinVarMap $ Map.insert (VarMain n) 1 vs) c
    go (LinExpr vs c) (ELit x) = LinExpr vs (c + x)
    go (LinExpr (LinVarMap vs) c) (ECoeff (EVar n) x) =
      LinExpr (LinVarMap $ Map.insert (VarMain n) x vs) c
    go le (EAdd e1 e2) = mergeLinExpr (go le e1) (go le e2)

-- | Merged duplicate @LinVar@s in a @LinExpr@. Should be used /after/ @addLin@.
removeDupLin :: LinExpr -> LinExpr
removeDupLin (LinExpr (LinVarMap vs) c) = LinExpr (LinVarMap $ foldr go Map.empty $ Map.toList vs) c
  where
    go :: (LinVarName, Rational) -> Map.Map LinVarName Rational -> Map.Map LinVarName Rational
    go (n, x) acc | acc == Map.empty = Map.singleton n x
                  | otherwise = case Map.lookup n acc of
      Just y -> Map.update (\x -> Just $ y + x) n acc
      Nothing -> Map.insert n x acc

makeLinExpr :: LinAst -> LinExpr
makeLinExpr = removeDupLin . addLin . multLin

-- * Linear Inequalities

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

(.==.) :: LinAst -> LinAst -> IneqExpr
x .==. y = EquExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .==.

(.<=.) :: LinAst -> LinAst -> IneqExpr
x .<=. y = LteExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .<=.

(.=>.) :: LinAst -> LinAst -> IneqExpr
(.=>.) = flip (.<=.)

infixl 7 .=>.

-- * Standard Form

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
  arbitrary = liftM2 Equ (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed = hasNoDups . names

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
  arbitrary = liftM2 Lte (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed = hasNoDups . names

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
  arbitrary = liftM2 Gte (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed = hasNoDups . names

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


-- | Turns a user-level AST to a structurally standard from inequality.
standardForm :: IneqExpr -> IneqStdForm
standardForm = go . standardize
  where
    go (EquExpr (LinExpr xs xc) (LinExpr ys yc)) | xs == mempty && yc == 0 = EquStd $ Equ ys xc
                                                 | ys == mempty && xc == 0 = EquStd $ Equ xs yc
    go (LteExpr (LinExpr xs xc) (LinExpr ys yc)) | xs == mempty && yc == 0 = GteStd $ Gte ys xc -- Ax >= M
                                                 | ys == mempty && xc == 0 = LteStd $ Lte xs yc -- Ax <= M
    go _ = error "Non-standard Ineq"

-- | Standardizes user-level inequalities - to be used before @standardForm@.
standardize :: IneqExpr -> IneqExpr
standardize (EquExpr (LinExpr xs xc) (LinExpr ys yc))
  | xs == mempty = EquExpr (LinExpr mempty (xc - yc)) (LinExpr ys 0)
  | ys == mempty = EquExpr (LinExpr xs 0) (LinExpr mempty (yc - xc))
  | otherwise =
      let ys' = mapCoeffs (map ((-1) *)) ys
      in EquExpr (removeDupLin $ LinExpr (ys' `union` xs) 0) (LinExpr mempty (yc - xc))
standardize (LteExpr (LinExpr xs xc) (LinExpr ys yc))
  | xs == mempty = LteExpr (LinExpr mempty (xc - yc)) (LinExpr ys 0)
  | ys == mempty = LteExpr (LinExpr xs 0) (LinExpr mempty (yc - xc))
  | otherwise =
      let ys' = mapCoeffs (map ((-1) *)) ys
      in LteExpr (removeDupLin $ LinExpr (ys' `union` xs) 0) (LinExpr mempty (yc - xc))


hasNoDups :: (Ord a) => [a] -> Bool
hasNoDups = loop Set.empty
  where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s
                    = loop s' xs
                  | otherwise
                    = False

between1000Rational :: Gen Rational
between1000Rational = arbitrary `suchThat` (\x -> x <= 1000 && x >= -1000)
