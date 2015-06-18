{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , TypeSynonymInstances
  , FlexibleInstances
  #-}

module Linear.Grammar where

import Linear.Grammar.Class

import Data.Char
import Data.List
import Data.String
import qualified Data.Set as Set
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
  mapNames f (EVar n) = EVar $ head $ f [n]
  mapNames _ (ELit x) = ELit x
  mapNames f (ECoeff e c) = ECoeff (mapNames f e) c
  mapNames f (EAdd e1 e2) = EAdd (mapNames f e1) (mapNames f e2)
  vars (EVar n) = [n]
  vars (ELit _) = []
  vars (ECoeff e _) = names e
  vars (EAdd e1 e2) = names e1 ++ names e2
  mapVars f (EVar n) = EVar $ head $ f [n]
  mapVars _ (ELit x) = ELit x
  mapVars f (ECoeff e c) = ECoeff (mapNames f e) c
  mapVars f (EAdd e1 e2) = EAdd (mapNames f e1) (mapNames f e2)

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

data LinVar = LinVar
  { varName  :: String
  , varCoeff :: Rational
  } deriving (Show, Eq)

instance HasVariables LinVar [LinVar] where
  names (LinVar n _) = [n]
  mapNames f (LinVar n x) = LinVar (head $ f [n]) x
  vars lv = [lv]
  mapVars f x = head $ f [x]

instance HasVariables [LinVar] [LinVar] where
  names = concatMap names
  mapNames f = map (mapNames f)
  vars = concatMap vars
  mapVars f = f

instance HasCoefficients LinVar where
  coeffVals (LinVar _ x) = [x]
  mapCoeffs f (LinVar n x) = LinVar n $ head $ f [x]

instance HasCoefficients [LinVar] where
  coeffVals = concatMap coeffVals
  mapCoeffs f = map (mapCoeffs f)

instance Arbitrary LinVar where
  arbitrary = liftM2 LinVar (arbitrary `suchThat` (\x -> length x < 5
                                                      && not (null x)
                                                      && all isAlpha x))
                            between1000Rational

-- | Name-based ordering
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: String -> LinVar -> Bool
hasName n (LinVar m _) = n == m

hasCoeff :: Rational -> LinVar -> Bool
hasCoeff x (LinVar _ y) = x == y


-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprVars :: [LinVar]
  , exprConst  :: Rational
  } deriving (Show, Eq)

instance HasVariables LinExpr [LinVar] where
  names (LinExpr xs _) = names xs
  mapNames f (LinExpr xs xc) = LinExpr (mapNames f xs) xc
  vars (LinExpr xs _) = vars xs
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
      isUniquelyNamed x = hasNoDups $ map varName x

mergeLinExpr :: LinExpr -> LinExpr -> LinExpr
mergeLinExpr (LinExpr vs1 x) (LinExpr vs2 y) = LinExpr (vs1 ++ vs2) (x + y)

-- | Turns @LinAst@ to @LinExpr@ - should be done /after/ @multLin@.
addLin :: LinAst -> LinExpr
addLin = go (LinExpr [] 0)
  where
    go :: LinExpr -> LinAst -> LinExpr
    go (LinExpr vs c) (EVar n)            = LinExpr (LinVar n 1:vs) c
    go (LinExpr vs c) (ELit x)            = LinExpr vs (c + x)
    go (LinExpr vs c) (ECoeff (EVar n) x) = LinExpr (LinVar n x:vs) c
    go le (EAdd e1 e2) = mergeLinExpr (go le e1) (go le e2)

-- | Merged duplicate @LinVar@s in a @LinExpr@. Should be used /after/ @addLin@.
removeDupLin :: LinExpr -> LinExpr
removeDupLin (LinExpr vs c) = LinExpr (foldr go [] vs) c
  where
    go :: LinVar -> [LinVar] -> [LinVar]
    go x [] = [x]
    go (LinVar n x) acc = case find (hasName n) acc of
      Just (LinVar m y) -> LinVar m (y + x):filter (not . hasName n) acc
      Nothing           -> LinVar n x:acc

makeLinExpr :: LinAst -> LinExpr
makeLinExpr = removeDupLin . addLin . multLin

-- * Linear Inequalities

data IneqExpr =
    EquExpr LinExpr LinExpr
  | LteExpr LinExpr LinExpr
  deriving (Show, Eq)

instance HasVariables IneqExpr [LinVar] where
  names (EquExpr x y) = names x ++ names y
  names (LteExpr x y) = names x ++ names y
  mapNames f (EquExpr x y) = EquExpr (mapNames f x) (mapNames f y)
  mapNames f (LteExpr x y) = LteExpr (mapNames f x) (mapNames f y)
  vars (EquExpr x y) = vars x ++ vars y
  vars (LteExpr x y) = vars x ++ vars y
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

data Equality = Equ [LinVar] Rational
  deriving (Show, Eq)

instance HasVariables Equality [LinVar] where
  names (Equ xs _) = names xs
  mapNames f (Equ xs xc) = Equ (mapNames f xs) xc
  vars (Equ xs _) = xs
  mapVars f (Equ xs xc) = Equ (f xs) xc

instance HasCoefficients Equality where
  coeffVals (Equ xs _) = coeffVals xs
  mapCoeffs f (Equ xs xc) = Equ (mapCoeffs f xs) xc

instance HasConstant Equality where
  constVal (Equ _ xc) = xc
  mapConst f (Equ xs xc) = Equ xs $ f xc

instance Arbitrary Equality where
  arbitrary = liftM2 Equ (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed x = hasNoDups $ map varName x

data LInequality = Lte [LinVar] Rational
  deriving (Show, Eq)

instance HasVariables LInequality [LinVar] where
  names (Lte xs _) = concatMap names xs
  mapNames f (Lte xs xc) = Lte (mapNames f xs) xc
  vars (Lte xs _) = xs
  mapVars f (Lte xs xc) = Lte (f xs) xc

instance HasCoefficients LInequality where
  coeffVals (Lte xs _) = coeffVals xs
  mapCoeffs f (Lte xs xc) = Lte (mapCoeffs f xs) xc

instance HasConstant LInequality where
  constVal (Lte _ xc) = xc
  mapConst f (Lte xs xc) = Lte xs $ f xc

instance Arbitrary LInequality where
  arbitrary = liftM2 Lte (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed x = hasNoDups $ map varName x

data GInequality = Gte [LinVar] Rational
  deriving (Show, Eq)

instance HasVariables GInequality [LinVar] where
  names (Gte xs _) = concatMap names xs
  mapNames f (Gte xs xc) = Gte (mapNames f xs) xc
  vars (Gte xs _) = xs
  mapVars f (Gte xs xc) = Gte (f xs) xc

instance HasCoefficients GInequality where
  coeffVals (Gte xs _) = coeffVals xs
  mapCoeffs f (Gte xs xc) = Gte (mapCoeffs f xs) xc

instance HasConstant GInequality where
  constVal (Gte _ xc) = xc
  mapConst f (Gte xs xc) = Gte xs $ f xc

instance Arbitrary GInequality where
  arbitrary = liftM2 Gte (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed x = hasNoDups $ map varName x

data IneqStdForm =
    EquStd Equality
  | LteStd LInequality
  | GteStd GInequality
  deriving (Show, Eq)

instance HasVariables IneqStdForm [LinVar] where
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
    go (EquExpr (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = EquStd $ Equ ys xc
                                                 | null ys && xc == 0 = EquStd $ Equ xs yc
    go (LteExpr (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = GteStd $ Gte ys xc -- Ax >= M
                                                 | null ys && xc == 0 = LteStd $ Lte xs yc -- Ax <= M
    go _ = error "Non-standard Ineq"

-- | Standardizes user-level inequalities - to be used before @standardForm@.
standardize :: IneqExpr -> IneqExpr
standardize (EquExpr (LinExpr xs xc) (LinExpr ys yc))
  | null xs   = EquExpr (LinExpr [] (xc - yc)) (LinExpr ys 0)
  | null ys   = EquExpr (LinExpr xs 0) (LinExpr [] (yc - xc))
  | otherwise =
      let
        ys' = mapCoeffs (map ((-1) *)) ys
      in
      EquExpr (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))
standardize (LteExpr (LinExpr xs xc) (LinExpr ys yc))
  | null xs   = LteExpr (LinExpr [] (xc - yc)) (LinExpr ys 0)
  | null ys   = LteExpr (LinExpr xs 0) (LinExpr [] (yc - xc))
  | otherwise =
      let
        ys' = mapCoeffs (map ((-1) *)) ys
      in
      LteExpr (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))

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
