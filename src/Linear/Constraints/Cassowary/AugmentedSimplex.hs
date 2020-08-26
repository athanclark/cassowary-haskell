{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , GADTs
  , TupleSections
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Prelude hiding (foldr, minimum, zip, lookup, filter)

import Linear.Constraints.Tableau (Tableau (Tableau))
import Linear.Constraints.Weights
  ( Weight (Weight)
  , subMapWeight
  , compressWeight
  )
import Linear.Grammar.Types
  ( Equality (Equ, getEqu)
  , IneqStdForm (EquStd, unEquStd)
  , ineqStdVars
  , ineqStdMapVars
  , ineqStdConst
  , ineqStdMapConst
  , subMap
  , linExprVars
  , linExprMapVars
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





-- TODO: Turn all of this into a class, overloaded by the coefficient type.
-- Also, objective function should be raw Rational

-- * Bland's Rule

-- ** Primal

newtype Snd a b = Snd {getSnd :: (a,b)}

instance Eq b => Eq (Snd a b) where
  (Snd (_,y1)) == (Snd (_,y2)) = y1 == y2

instance (Ord b) => Ord (Snd a b) where
  (Snd (_,y1)) `compare` (Snd (_,y2)) = compare y1 y2


-- | Most negative coefficient in objective function
nextBasicPrimalRational :: ( Ord a
                           , Num a
                           ) => Equality k a Rational -- ^ Objective function
                             -> Maybe k
nextBasicPrimalRational (Equ objective) = do
  -- gets the first successful, minimal result
  let Option mKeyVal =
        Map.foldMapWithKey
          (\var coeff -> Option $ Just $ Min $ Snd (var,coeff))
          (linExprVars objective)
  (var,coeff) <- (getSnd . getMin) <$> mKeyVal
  guard (coeff < 0) -- Must be positive, but the /most/ negative out of the set
  return var

nextBasicPrimalWeight :: ( Ord a
                         , Num a
                         ) => Equality k (Weight a) Rational
                           -> Maybe k
nextBasicPrimalWeight (Equ objective) = do
  go 0
  where
    go n
      | all (\(Weight x) -> length x <= n) (linExprVars objective) = Nothing -- base case
      | otherwise =
          let currentResult =
                nextBasicPrimalRational $
                  Equ $
                    linExprMapVars (Map.mapMaybe (\(Weight y) -> y V.!? n)) objective
              recurseResult = go (n + 1)
          in  getFirst (First currentResult <> First recurseResult)



-- | Finds the index of the next row to pivot on
nextRowPrimal :: Ord k
              => k
              -> IntMap.IntMap (IneqStdForm k Rational Rational)
              -> Maybe Int
nextRowPrimal var xs = do
  let (Option mKeyVal) =
        let go slack row = Option $ (\ratio -> Min $ Snd (slack,ratio)) <$> blandRatioPrimal var row
        in  IntMap.foldMapWithKey go xs
  (slack,_) <- (getSnd . getMin) <$> mKeyVal
  pure slack


-- | Bland's method.
blandRatioPrimal :: Ord k
                 => k
                 -> IneqStdForm k Rational Rational
                 -> Maybe Rational
blandRatioPrimal var row = do
  coeff <- Map.lookup var (ineqStdVars row)
  guard (coeff < 0)
  pure (negate (ineqStdConst row) / coeff)


-- ** Dual

nextBasicDualRational :: ( Ord k
                         , Ord a
                         , Num a
                         , Fractional a
                         ) => Equality k a Rational
                           -> IneqStdForm k a Rational
                           -> Maybe k
nextBasicDualRational objective row = do
  let osMap = linExprVars (getEqu objective)
      xsMap = ineqStdVars row
      allVars = osMap <> xsMap
      (Option mKeyVar) =
        let go var _ = Option $ (\ratio -> Min $ Snd (var,ratio)) <$> blandRatioDualRational var objective row
        in  Map.foldMapWithKey go allVars
  (var,_) <- (getSnd . getMin) <$> mKeyVar
  pure var

nextBasicDualWeight :: ( Ord k
                       , Ord a
                       , Num a
                       , Fractional a
                       ) => Equality k (Weight a) Rational
                         -> IneqStdForm k a Rational
                         -> Maybe k
nextBasicDualWeight (Equ objective) row =
  go 0
  where
    go n | all (\(Weight x) -> length x <= n) $ linExprVars objective = Nothing
         | otherwise = getFirst $
             First (nextBasicDualRational
                      (Equ $ linExprMapVars (Map.mapMaybe (\(Weight y) -> y V.!? n)) objective)
                      row)
          <> First (go $ n+1)


blandRatioDualRational :: ( Ord k
                          , Ord a
                          , Num a
                          , Fractional a
                          ) => k
                            -> Equality k a Rational
                            -> IneqStdForm k a Rational
                            -> Maybe a
blandRatioDualRational var (Equ objective) row = do
  let o = fromMaybe 0 $ Map.lookup var $ linExprVars objective
  x <- Map.lookup var $ ineqStdVars row
  guard $ x > 0
  return $ o / x


nextRowDual :: IntMap.IntMap (IneqStdForm k Rational Rational) -> Maybe Int
nextRowDual xs = do
  let (Option mKeyVal) = IntMap.foldMapWithKey
                             (\slack row -> Option (Just (Min (Snd (slack,ineqStdConst row)))))
                             xs
  (slack,const) <- (getSnd . getMin) <$> mKeyVal
  guard $ const < 0
  return slack

-- * Equation Refactoring

-- | Orients / refactors an equation for one of its variables
flatten :: Ord k => k -> IneqStdForm k Rational Rational -> IneqStdForm k Rational Rational
flatten var row = case Map.lookup var (ineqStdVars row) of
  Just coeff -> ineqStdMapConst (/ coeff) (ineqStdMapVars (fmap (/ coeff)) row)
  Nothing    -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
substituteRational :: Ord k
                   => k
                   -> IneqStdForm k Rational Rational -- ^ Replacement
                   -> IneqStdForm k Rational Rational -- ^ Subject
                   -> IneqStdForm k Rational Rational
substituteRational var replacement target =
  case Map.lookup var $ ineqStdVars target of
    Just existingCoeff ->
      let magnifiedReplacement =
            let mapCoeffs :: IneqStdForm k Rational Rational -> IneqStdForm k Rational Rational
                mapCoeffs = ineqStdMapVars (fmap (existingCoeff *))
            in  mapCoeffs (ineqStdMapConst (existingCoeff *) replacement)
      in  ineqStdMapVars  (`subMap` (ineqStdVars magnifiedReplacement))
            (ineqStdMapConst (subtract (ineqStdConst magnifiedReplacement)) target)
    Nothing -> target

substituteWeight :: Ord k
                 => k
                 -> IneqStdForm k Rational Rational -- ^ Replacement
                 -> IneqStdForm k (Weight Rational) Rational -- ^ Subject
                 -> IneqStdForm k (Weight Rational) Rational
substituteWeight var replacement target =
  case Map.lookup var (ineqStdVars target) of
    Just existingCoeff ->
      let magnifiedReplacement =
            let mapCoeffs :: IneqStdForm k Rational Rational -> IneqStdForm k (Weight Rational) Rational
                mapCoeffs = ineqStdMapVars (fmap (\c -> fmap (c *) existingCoeff))
            in  mapCoeffs (ineqStdMapConst ((compressWeight existingCoeff) *) replacement)
      in  ineqStdMapVars (`subMapWeight` (ineqStdVars magnifiedReplacement))
            (ineqStdMapConst (subtract (ineqStdConst magnifiedReplacement)) target)
    Nothing -> target


-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimalWith :: Ord k
                => (Equality k a Rational -> Maybe k) -- ^ Next basic variable
                -> (k -> IneqStdForm k Rational Rational -> Equality k a Rational -> Equality k a Rational) -- ^ Substitution
                -> (Tableau k k Rational Rational, Equality k a Rational)
                -> Maybe (Tableau k k Rational Rational, Equality k a Rational)
pivotPrimalWith nextBasic substituteObj (Tableau cBasic cSlack, objective) = do
  var   <- nextBasic objective
  slack <- nextRowPrimal var cSlack
  row   <- IntMap.lookup slack cSlack
  let replacement = flatten var row
      final       = ineqStdMapVars (Map.delete var) replacement
  return ( Tableau (Map.insert var final $
                      substituteRational var replacement <$> cBasic)
                   (substituteRational var replacement <$> IntMap.delete slack cSlack)
         , substituteObj var replacement objective
         )


pivotDualWith :: Ord k
              => (Equality k a Rational -> IneqStdForm k Rational Rational -> Maybe k) -- ^ Next basic variable
              -> (k -> IneqStdForm k Rational Rational -> Equality k a Rational -> Equality k a Rational) -- ^ Substitution
              -> (Tableau k k Rational Rational, Equality k a Rational)
              -> Maybe (Tableau k k Rational Rational, Equality k a Rational)
pivotDualWith nextBasic substituteObj (Tableau cBasic cSlack, objective) = do
  slack <- nextRowDual cSlack
  row   <- IntMap.lookup slack cSlack
  var   <- nextBasic objective row
  let replacement = flatten var row
      final       = ineqStdMapVars (Map.delete var) replacement
  return ( Tableau (Map.insert var final $
                      substituteRational var replacement <$> cBasic)
                   (substituteRational var replacement <$> IntMap.delete slack cSlack)
         , substituteObj var replacement objective
         )

-- | Optimize when given a pivot function
simplexWith :: ((Tableau k0 k1 Rational Rational, Equality k2 a Rational) -> Maybe (Tableau k0 k1 Rational Rational, Equality k2 a Rational))
            -> (Tableau k0 k1 Rational Rational, Equality k2 a Rational)
            -> (Tableau k0 k1 Rational Rational, Equality k2 a Rational)
simplexWith piv x =
 case piv x of
   Just (cs,f) -> simplexWith piv (cs,f)
   Nothing -> x

-- | Primal maximizing optimization
simplexPrimalRational :: Ord k
                      => (Tableau k k Rational Rational, Equality k Rational Rational)
                      -> (Tableau k k Rational Rational, Equality k Rational Rational)
simplexPrimalRational = simplexWith (pivotPrimalWith nextBasicPrimalRational (\a b c -> unEquStd $ substituteRational a b $ EquStd c))

-- | Dual minimizing optimization
simplexDualRational :: Ord k
                    => (Tableau k k Rational Rational, Equality k Rational Rational)
                    -> (Tableau k k Rational Rational, Equality k Rational Rational)
simplexDualRational = simplexWith (pivotDualWith nextBasicDualRational (\a b c -> unEquStd $ substituteRational a b $ EquStd c))


