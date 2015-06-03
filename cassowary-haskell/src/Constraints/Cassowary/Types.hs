module Constraints.Cassowary.Types where

import Data.List (find)


data LinAst =
    EVar String
  | ELit Double
  | ECoeff LinAst Double
  | EAdd LinAst LinAst
  deriving (Show, Eq)

(.*.) = ECoeff
infixr 9 .*.

(.+.) = EAdd
infixr 8 .+.

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

data LinVar = LinVar
  { varName  :: String
  , varCoeff :: Double
  } deriving (Show, Eq)

hasName :: LinVar -> LinVar -> Bool
hasName (LinVar n _) (LinVar m _) = n == m

data LinExpr = LinExpr
  { exprCoeffs :: [LinVar]
  , exprConst  :: Double
  } deriving (Show, Eq)

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

-- | Merged duplicate @LinVar@s in a @LinExpr@.
removeDupLin :: LinExpr -> LinExpr
removeDupLin (LinExpr vs c) = LinExpr (foldr go [] vs) c
  where
    go :: LinVar -> [LinVar] -> [LinVar]
    go x [] = [x]
    go (LinVar n x) xs = case find (hasName $ LinVar n x) xs of
      Just (LinVar m y) -> LinVar m (y + x):filter (not . hasName (LinVar n x)) xs
      Nothing           -> xs
