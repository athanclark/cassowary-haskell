module Linear.Grammar
  ( module X
-- * User-facing API
  , multLin
-- * Linear Expressions
  , addLin
-- * Linear Inequalities
  , (.==.)
  , (.<=.)
  , (.=>.)
  , makeLinExpr
-- * Standard Form
  , standardForm
  , hasNoDups
  ) where

import Linear.Grammar.Types as X

import Data.Semigroup
import qualified Data.Set as Set
import qualified Data.Map as Map


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

-- | Turns @LinAst@ to @LinExpr@ - should be done /after/ @multLin@.
addLin :: LinAst -> (LinExpr String Rational)
addLin = go (LinExpr Map.empty 0)
  where
    go :: LinExpr String Rational -> LinAst -> LinExpr String Rational
    go (LinExpr vs c) (EVar n) =
      LinExpr (maybe (Map.insert n 1 vs)
                     (\coeff -> Map.insert n (coeff + 1) vs) $
                  Map.lookup n vs) c
    go (LinExpr vs c) (ELit x) = LinExpr vs (c + x)
    go acc (ECoeff _ 0) = acc
    go (LinExpr vs c) (ECoeff (EVar n) x) =
      LinExpr (maybe (Map.insert n x vs)
                     (\coeff -> Map.insert n (coeff + x) vs) $
                  Map.lookup n vs) c
    go le (EAdd e1 e2) = go le e1 <> go le e2
    go _ _ = error "`addLin` was used on unprocessed input."


(.==.) :: LinAst -> LinAst -> IneqExpr String Rational
x .==. y = EquExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .==.

(.<=.) :: LinAst -> LinAst -> IneqExpr String Rational
x .<=. y = LteExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .<=.

(.=>.) :: LinAst -> LinAst -> IneqExpr String Rational
(.=>.) = flip (.<=.)

infixl 7 .=>.

makeLinExpr :: LinAst -> LinExpr String Rational
makeLinExpr = addLin . multLin

-- | Turns a user-level AST to a structurally standard from inequality.
standardForm :: IneqExpr String Rational -> IneqStdForm String Rational
standardForm = go . standardize
  where
    go (EquExpr (LinExpr xs xc) (LinExpr ys yc)) | xs == mempty && yc == 0 = EquStd $ Equ $ LinExpr ys xc
                                                 | ys == mempty && xc == 0 = EquStd $ Equ $ LinExpr xs yc
    go (LteExpr (LinExpr xs xc) (LinExpr ys yc)) | xs == mempty && yc == 0 = GteStd $ Gte $ LinExpr ys xc -- Ax >= M
                                                 | ys == mempty && xc == 0 = LteStd $ Lte $ LinExpr xs yc -- Ax <= M
    go _ = error "Non-standard Ineq"

    -- Standardizes user-level inequalities - to be used before @standardForm@.
    standardize :: IneqExpr String Rational -> IneqExpr String Rational
    standardize (EquExpr (LinExpr xs xc) (LinExpr ys yc))
      | xs == mempty = EquExpr (LinExpr mempty (xc - yc)) (LinExpr ys 0)
      | ys == mempty = EquExpr (LinExpr xs 0) (LinExpr mempty (yc - xc))
      | otherwise =
          let ys' = negate <$> ys
          in EquExpr (LinExpr (ys' <> xs) 0) (LinExpr mempty (yc - xc))
    standardize (LteExpr (LinExpr xs xc) (LinExpr ys yc))
      | xs == mempty = LteExpr (LinExpr mempty (xc - yc)) (LinExpr ys 0)
      | ys == mempty = LteExpr (LinExpr xs 0) (LinExpr mempty (yc - xc))
      | otherwise =
          let ys' = negate <$> ys
          in LteExpr (LinExpr (ys' <> xs) 0) (LinExpr mempty (yc - xc))


hasNoDups :: Ord a => [a] -> Bool
hasNoDups = loop Set.empty
  where
    loop _ []       = True
    loop s (x:xs) | s' <- Set.insert x s, Set.size s' > Set.size s = loop s' xs
                  | otherwise = False
