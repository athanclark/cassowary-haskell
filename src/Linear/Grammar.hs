{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Grammar
  (
-- * User-facing API
    multLin
-- * Linear Expressions
  , addLin
-- * Linear Inequalities
  , (.==.)
  , (.<=.)
  , (.=>.)
  , makeLinExpr
-- * Standard Form
  , standardForm
  ) where

import Linear.Grammar.Types
  ( LinAst (ELit, EVar, ECoeff, EAdd)
  , LinExpr (LinExpr)
  , IneqExpr (EquExpr, LteExpr)
  , IneqStdForm (EquStd, LteStd, GteStd)
  , Equality (Equ)
  , LInequality (Lte)
  , GInequality (Gte)
  )

import qualified Data.Map as Map


-- | Pushes 'ECoeff' down the tree, leaving 'EAdd' at the top level.
-- After using this funciton, all 'ECoeff' constructors\' 'LinAst' parameter will
-- be an 'EVar'.
multLin :: Num a => LinAst k a -> LinAst k a
multLin (EVar n) = EVar n
multLin (ELit x) = ELit x
multLin (ECoeff e x) = case multLin e of
  (ELit y)      -> ELit (y * x) -- evaluate coefficient multiplied by literal
  (EVar n)      -> ECoeff (EVar n) x -- do nothing
  (ECoeff e' y) -> ECoeff e' (y * x) -- redundant coefficients
  (EAdd e1 e2)  -> EAdd (multLin (ECoeff e1 x)) (multLin (ECoeff e2 x)) -- recurse
multLin (EAdd e1 e2) = EAdd (multLin e1) (multLin e2)

-- | Turns 'LinAst' to 'LinExpr' - should be done /after/ 'multLin'.
addLin :: forall k a. Eq a => Num a => Ord k => LinAst k a -> LinExpr k a a
addLin = go mempty
  where
    -- fold over the LinAst, with an empty initial accumulator
    go :: LinExpr k a a -> LinAst k a -> LinExpr k a a
    go (LinExpr vs c) (EVar n) =
      let onLookup = maybe (Map.insert n 1 vs)
                       (\coeff -> Map.insert n (coeff + 1) vs)
      in  LinExpr (onLookup (Map.lookup n vs)) c
    go (LinExpr vs c) (ELit x) = LinExpr vs (c + x)
    go acc (ECoeff _ 0) = acc
    go (LinExpr vs c) (ECoeff (EVar n) x) =
      LinExpr (maybe (Map.insert n x vs)
                     (\coeff -> Map.insert n (coeff + x) vs) $
                  Map.lookup n vs) c
    go le (EAdd e1 e2) = go le e1 <> go le e2
    go _ _ = error "`addLin` was used on unprocessed input."


(.==.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> IneqExpr k a a
x .==. y = EquExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .==.

(.<=.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> IneqExpr k a a
x .<=. y = LteExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .<=.

(.=>.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> IneqExpr k a a
(.=>.) = flip (.<=.)

infixl 7 .=>.

makeLinExpr :: Eq a => Num a => Ord k => LinAst k a -> LinExpr k a a
makeLinExpr = addLin . multLin

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
        | Map.null xs && yc == 0 -> GteStd $ Gte $ LinExpr ys xc -- Ax >= M
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
              in (LinExpr (ys' <> xs) 0, LinExpr mempty (yc - xc))
