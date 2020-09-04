{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Constraints.Cassowary.Refactor where

import Linear.Grammar.Types
  ( IneqStdForm
  , ineqStdVars
  , ineqStdConst
  , ineqStdMapConst
  , ineqStdMapVars
  , subMap
  )
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
flatten var row = case Map.lookup var (ineqStdVars row) of
  Just coeff -> ineqStdMapConst (/ coeff) (ineqStdMapVars (fmap (/ coeff)) row)
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
  case Map.lookup var (ineqStdVars target) of
    Just existingCoeff ->
      let magnifiedReplacement =
            let mapCoeffs :: IneqStdForm k a c -> IneqStdForm k a c
                mapCoeffs = ineqStdMapVars (fmap (existingCoeff *))
            in  mapCoeffs (ineqStdMapConst (existingCoeff *) replacement)
      in  ineqStdMapVars  (`subMap` (ineqStdVars magnifiedReplacement))
            (ineqStdMapConst (subtract (ineqStdConst magnifiedReplacement)) target)
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
  case Map.lookup var (ineqStdVars target) of
    Just existingCoeff ->
      let magnifiedReplacement =
            let mapCoeffs :: IneqStdForm k a c -> IneqStdForm k (Weight a) c
                mapCoeffs = ineqStdMapVars (fmap (\c -> fmap (c *) existingCoeff))
                existingCoeff' :: a
                existingCoeff' = compressWeight existingCoeff
            in  mapCoeffs (ineqStdMapConst (existingCoeff' *) replacement)
      in  ineqStdMapVars (`subMapWeight` (ineqStdVars magnifiedReplacement))
            (ineqStdMapConst (subtract (ineqStdConst magnifiedReplacement)) target)
    Nothing -> target
