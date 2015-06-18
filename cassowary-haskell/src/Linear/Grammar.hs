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

-- | For sorting tableaus
instance Ord LinVar where
  compare (LinVar x _) (LinVar y _) = compare x y

hasName :: String -> LinVar -> Bool
hasName n (LinVar m _) = n == m

mapName :: (String -> String) -> LinVar -> LinVar
mapName f (LinVar n x) = LinVar (f n) x

hasCoeff :: Rational -> LinVar -> Bool
hasCoeff x (LinVar _ y) = x == y

mapCoeff :: (Rational -> Rational) -> LinVar -> LinVar
mapCoeff f (LinVar n x) = LinVar n $ f x

-- | Linear expressions suited for normal and standard form.
data LinExpr = LinExpr
  { exprVars :: [LinVar]
  , exprConst  :: Rational
  } deriving (Show, Eq)

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
  names (Equ xs _) = concatMap names xs
  mapNames f (Equ xs xc) = Equ (mapNames f xs) xc
  vars (Equ xs _) = xs
  mapVars f (Equ xs xc) = Equ (f xs) xc

instance Arbitrary Equality where
  arbitrary = liftM2 Equ (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed x = hasNoDups $ map varName x

data LInequality = Lte [LinVar] Rational
  deriving (Show, Eq)

instance Arbitrary LInequality where
  arbitrary = liftM2 Lte (arbitrary `suchThat` isUniquelyNamed) between1000Rational
    where
      isUniquelyNamed x = hasNoDups $ map varName x

instance HasVariables LInequality [LinVar] where
  names (Lte xs _) = concatMap names xs
  mapNames f (Lte xs xc) = Lte (mapNames f xs) xc
  vars (Lte xs _) = xs
  mapVars f (Lte xs xc) = Lte (f xs) xc

data GInequality = Gte [LinVar] Rational
  deriving (Show, Eq)

instance HasVariables GInequality [LinVar] where
  names (Gte xs _) = concatMap names xs
  mapNames f (Gte xs xc) = Gte (mapNames f xs) xc
  vars (Gte xs _) = xs
  mapVars f (Gte xs xc) = Gte (f xs) xc

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

instance Arbitrary IneqStdForm where
  arbitrary = oneof
    [ EquStd <$> arbitrary
    , LteStd <$> arbitrary
    , GteStd <$> arbitrary
    ]


-- mapStdVars :: ([LinVar] -> [LinVar]) -> IneqStdForm -> IneqStdForm
-- mapStdVars f (EquStd xs xc) = EquStd (f xs) xc
-- mapStdVars f (LteStd xs xc) = LteStd (f xs) xc
-- mapStdVars f (GteStd xs xc) = GteStd (f xs) xc
--
-- getStdConst :: IneqStdForm -> Rational
-- getStdConst (EquStd _ x) = x
-- getStdConst (LteStd _ x) = x
-- getStdConst (GteStd _ x) = x
--
-- mapStdConst :: (Rational -> Rational) -> IneqStdForm -> IneqStdForm
-- mapStdConst f (EquStd xs xc) = EquStd xs (f xc)
-- mapStdConst f (LteStd xs xc) = LteStd xs (f xc)
-- mapStdConst f (GteStd xs xc) = GteStd xs (f xc)

-- | Turns a user-level AST to a structurally standard from inequality.
-- standardForm :: IneqExpr -> IneqStdForm
-- standardForm = go . standardize
--   where
--     go (EquExpr (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = EquStd ys xc
--                                                  | null ys && xc == 0 = EquStd xs yc
--     go (LteExpr (LinExpr xs xc) (LinExpr ys yc)) | null xs && yc == 0 = GteStd ys xc -- Ax >= M
--                                                  | null ys && xc == 0 = LteStd xs yc -- Ax <= M
--     go _ = error "Non-standard Ineq"
--
-- -- | Standardizes user-level inequalities - to be used before @standardForm@.
-- standardize :: IneqExpr -> IneqExpr
-- standardize (EquExpr (LinExpr xs xc) (LinExpr ys yc))
--   | null xs   = EquExpr (LinExpr [] (xc - yc)) (LinExpr ys 0)
--   | null ys   = EquExpr (LinExpr xs 0) (LinExpr [] (yc - xc))
--   | otherwise =
--       let
--         ys' = map (mapCoeff $ (*) (-1)) ys
--       in
--       EquExpr (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))
-- standardize (LteExpr (LinExpr xs xc) (LinExpr ys yc))
--   | null xs   = LteExpr (LinExpr [] (xc - yc)) (LinExpr ys 0)
--   | null ys   = LteExpr (LinExpr xs 0) (LinExpr [] (yc - xc))
--   | otherwise =
--       let
--         ys' = map (mapCoeff $ (*) (-1)) ys
--       in
--       LteExpr (removeDupLin $ LinExpr (ys' ++ xs) 0) (LinExpr [] (yc - xc))

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
