{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Grammar where

import Linear.Grammar.Types.Syntax (LinAst (EAdd, ECoeff))
import Linear.Grammar.Types.Expressions (makeLinExpr)
import Linear.Grammar.Types.Inequalities (IneqExpr (EquExpr, LteExpr))



-- | Addition operation
(.+.) :: LinAst k a -> LinAst k a -> LinAst k a
(.+.) = EAdd

infixl 6 .+.

-- | Multiplication operation
(.*.) :: LinAst k a -> a -> LinAst k a
(.*.) = ECoeff

infixl 5 .*.

(.==.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> IneqExpr k a a
x .==. y = EquExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .==.

(.<=.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> IneqExpr k a a
x .<=. y = LteExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .<=.

(.>=.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> IneqExpr k a a
(.>=.) = flip (.<=.)

infixl 7 .>=.
