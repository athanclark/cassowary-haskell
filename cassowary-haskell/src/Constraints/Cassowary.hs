module Constraints.Cassowary
  ( module X
  ) where

import Constraints.Cassowary.Types as X


type Weight = Integer

data Constraint =
    EquivConstr Weight LinExpr LinExpr
  | LteConstr   Weight LinExpr LinExpr
  | GteConstr   Weight LinExpr LinExpr
  deriving (Show, Eq)
