{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Constraints.Cassowary.Refactor where

import Linear.Grammar.Types.Class (getVars, getConst, mapConst, mapVars, mapAllVars)
import Linear.Grammar.Types.Utilities (subMap)
import Linear.Grammar.Types.Inequalities (IneqStdForm)
import Linear.Constraints.Weights
  ( Weight
  , compressWeight
  , subMapWeight
  )

import qualified Data.Map as Map



-- * Equation Refactoring

-- | Orients / refactors an equation for one of its variables
flatten :: ( Ord k
           , Fractional a
           ) => k
             -> IneqStdForm k a a
             -> IneqStdForm k a a
flatten var row = case Map.lookup var (getVars row) of
  Just coeff -> mapConst (/ coeff) (mapVars (/ coeff) row)
  Nothing    -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
substitute :: forall k a
            . ( Ord k
              , Num a
              , Eq a
              ) => k
                -> IneqStdForm k a a -- ^ Replacement
                -> IneqStdForm k a a -- ^ Subject
                -> IneqStdForm k a a
substitute var replacement target =
  case Map.lookup var (getVars target) of
    Just existingCoeff ->
      let magnifiedReplacement =
            mapVars (existingCoeff *) (mapConst (existingCoeff *) replacement)
      in  mapAllVars (`subMap` (getVars magnifiedReplacement))
            (mapConst (subtract (getConst magnifiedReplacement)) target)
    Nothing -> target

substituteWeight :: forall k a
                  . ( Ord k
                    , Num a
                    , Eq a
                    ) => k
                      -> IneqStdForm k a a -- ^ Replacement
                      -> IneqStdForm k (Weight a) a -- ^ Subject
                      -> IneqStdForm k (Weight a) a
substituteWeight var replacement target =
  case Map.lookup var (getVars target) of
    Just existingCoeff ->
      let magnifiedReplacement =
            let mapCoeffs :: IneqStdForm k a c -> IneqStdForm k (Weight a) c
                mapCoeffs = mapVars (\c -> fmap (c *) existingCoeff) -- FIXME use a proper weighted mult?
                existingCoeff' :: a
                existingCoeff' = compressWeight existingCoeff
            in  mapCoeffs (mapConst (existingCoeff' *) replacement)
      in  mapAllVars (`subMapWeight` (getVars magnifiedReplacement))
            (mapConst (subtract (getConst magnifiedReplacement)) target)
    Nothing -> target
