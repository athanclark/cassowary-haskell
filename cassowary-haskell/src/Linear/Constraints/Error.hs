{-# LANGUAGE
    DeriveFunctor
  #-}

module Linear.Constraints.Error where

import Linear.Constraints.Class
import Linear.Grammar
import Linear.Grammar.Class



-- * Error Variables

-- errorSubst :: String -> ([LinVar], Rational) -> Map.Map
-- errorSubst n (xs,xc) =
