{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Grammar
  ( (.+.)
  , (.*.)
  , (.==.)
  , (.<=.)
  , (.>=.)
  , module Linear.Grammar.Types.Inequalities
  )where

import Linear.Grammar.Types.Syntax (LinAst (EAdd, ECoeff))
import Linear.Grammar.Types.Expressions (makeLinExpr)
import qualified Linear.Grammar.Types.Inequalities as Ineq
import Linear.Grammar.Types.Inequalities (standardForm)


-- | Addition operation
(.+.) :: LinAst k a -> LinAst k a -> LinAst k a
(.+.) = EAdd

infixl 6 .+.

-- | Multiplication operation
(.*.) :: LinAst k a -> a -> LinAst k a
(.*.) = ECoeff

infixl 5 .*.

(.==.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> Ineq.IneqExpr k a a
x .==. y = Ineq.EquExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .==.

(.<=.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> Ineq.IneqExpr k a a
x .<=. y = Ineq.LteExpr (makeLinExpr x) (makeLinExpr y)

infixl 7 .<=.

(.>=.) :: Eq a => Num a => Ord k => LinAst k a -> LinAst k a -> Ineq.IneqExpr k a a
(.>=.) = flip (.<=.)

infixl 7 .>=.
