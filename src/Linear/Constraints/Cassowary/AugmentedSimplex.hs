{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , GADTs
  , TupleSections
  , RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Prelude hiding (foldr, minimum, zip, lookup, filter)

import Linear.Constraints.Cassowary.Refactor (flatten, substitute)
import Linear.Constraints.Cassowary.Basic
  ( nextRowPrimal
  , nextRowDual
  , nextBasicPrimal
  , nextBasicDual
  )
import Linear.Constraints.Tableau (Tableau (Tableau))
import Linear.Constraints.Weights
  ( Weight (Weight)
  , subMapWeight
  , compressWeight
  )
import Linear.Grammar.Types.Class (getVars, mapVars, getConst, mapConst, mapAllVars)
import Linear.Grammar.Types.Utilities (subMap)
import Linear.Grammar.Types.Inequalities
  ( Equality (Equ, getEqu)
  , IneqStdForm (EquStd, unEquStd)
  )

import Data.Maybe (fromMaybe) -- hiding (mapMaybe, catMaybes)
import Data.Semigroup
  ( Option (Option)
  , Min (Min, getMin)
  , First (First, getFirst)
  )
import qualified Data.Vector as V -- hiding (length, all)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad (guard)





-- | Finds an optimal solution from one that's already feasible - maximizing.
simplexPrimal :: forall k a a' c c'
               . ( Ord k
                 , Fractional a
                 , Ord a
                 ) => (Tableau k k a a, Equality k a a)
                   -> (Tableau k k a a, Equality k a a)
simplexPrimal = fixWhile pivot
  where
    go :: k -> IneqStdForm k a a -> Equality k a a -> Equality k a a
    go var replacement target = unEquStd (substitute var replacement (EquStd target))
    pivot :: (Tableau k k a a, Equality k a a) -> Maybe (Tableau k k a a, Equality k a a)
    pivot = pivotPrimalWith nextBasicPrimal go

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimalWith :: forall k a a' c c'
                 . ( Ord k
                   , Fractional a
                   , Ord a
                   ) => (Equality k a' c' -> Maybe k) -- ^ Next basic variable
                     -> (k -> IneqStdForm k a a -> Equality k a' c' -> Equality k a' c') -- ^ Substitution
                     -> (Tableau k k a a, Equality k a' c') -- ^ Tableau and objective
                     -> Maybe (Tableau k k a a, Equality k a' c')
pivotPrimalWith nextBasic substituteObj (Tableau cBasic cSlack, objective) = do
  (var :: k) <- nextBasic objective
  slack <- nextRowPrimal var cSlack
  row <- IntMap.lookup slack cSlack
  let replacement :: IneqStdForm k a a
      replacement = flatten var row
      final = mapAllVars (Map.delete var) replacement
  pure
    ( Tableau
        (Map.insert var final $ substitute var replacement <$> cBasic)
        (substitute var replacement <$> IntMap.delete slack cSlack)
    , substituteObj var replacement objective
    )


-- | Finds a feasible solution from one that's already optimal - minimizing.
simplexDual :: ( Ord k
               , Fractional a
               , Ord a
               ) => (Tableau k k a a, Equality k a a)
                 -> (Tableau k k a a, Equality k a a)
simplexDual = fixWhile pivot
  where
    go var replacement target = unEquStd (substitute var replacement (EquStd target))
    pivot = pivotDualWith nextBasicDual go

pivotDualWith :: forall k a a' c c'
               . ( Ord k
                 , Fractional a
                 , Ord a
                 ) => (Equality k a' c' -> IneqStdForm k a a -> Maybe k) -- ^ Next basic variable
                   -> (k -> IneqStdForm k a a -> Equality k a' c' -> Equality k a' c') -- ^ Substitution
                   -> (Tableau k k a a, Equality k a' c')
                   -> Maybe (Tableau k k a a, Equality k a' c')
pivotDualWith nextBasic substituteObj (Tableau cBasic cSlack, objective) = do
  slack <- nextRowDual cSlack
  row <- IntMap.lookup slack cSlack
  (var :: k) <- nextBasic objective row
  let replacement :: IneqStdForm k a a
      replacement = flatten var row
      final       = mapAllVars (Map.delete var) replacement
  pure
    ( Tableau
        (Map.insert var final $ substitute var replacement <$> cBasic)
        (substitute var replacement <$> IntMap.delete slack cSlack)
    , substituteObj var replacement objective
    )




fixWhile :: (a -> Maybe a) -> a -> a
fixWhile f x =
 case f x of
   Just y -> fixWhile f y
   Nothing -> x
