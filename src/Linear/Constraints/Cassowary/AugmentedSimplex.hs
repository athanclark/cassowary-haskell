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

import Linear.Constraints.Tableau
import Linear.Constraints.Weights
import Linear.Grammar

import Data.Maybe hiding (mapMaybe, catMaybes)
import Data.Semigroup
import Data.Vector as V hiding (length, all)
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Control.Monad





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
                           ) => Equality k a
                             -> Maybe k
nextBasicPrimalRational (Equ objective) = do
  let (Option mKeyVal) = Map.foldMapWithKey
                             (\var coeff -> Option $ Just $ Min $ Snd (var,coeff))
                             (linExprVars objective)
  (var,coeff) <- (getSnd . getMin) <$> mKeyVal
  guard $ coeff < 0
  return var

nextBasicPrimalWeight :: ( Ord a
                         , Num a
                         ) => Equality k (Weight a)
                           -> Maybe k
nextBasicPrimalWeight (Equ objective) = do
  go 0
  where
    go n | all (\(Weight x) -> length x <= n) $ linExprVars objective = Nothing
         | otherwise = getFirst $
             First (nextBasicPrimalRational $
               Equ $ linExprMapVars (Map.mapMaybe (\(Weight y) -> y !? n)) objective)
          <> First (go $ n+1)



-- | Finds the index of the next row to pivot on
nextRowPrimal :: ( Ord k
                 ) => k
                   -> IntMap.IntMap (IneqStdForm k Rational)
                   -> Maybe Int
nextRowPrimal var xs = do
  let (Option mKeyVal) = IntMap.foldMapWithKey
                             (\slack row -> Option $ (\ratio -> Min $ Snd (slack,ratio)) <$>
                                                         blandRatioPrimal var row)
                             xs
  (slack,_) <- (getSnd . getMin) <$> mKeyVal
  return slack


-- | Bland's method.
blandRatioPrimal :: ( Ord k
                    ) => k
                      -> IneqStdForm k Rational
                      -> Maybe Rational
blandRatioPrimal var row = do
  coeff <- Map.lookup var $ ineqStdVars row
  guard $ coeff < 0
  return $ negate (ineqStdConst row) / coeff


-- ** Dual

nextBasicDualRational :: ( Ord k
                         , Ord a
                         , Num a
                         , Fractional a
                         ) => Equality k a
                           -> IneqStdForm k a
                           -> Maybe k
nextBasicDualRational objective row =
  let osMap = linExprVars (getEqu objective)
      xsMap = ineqStdVars row
      allVars = osMap <> xsMap
  in do let (Option mKeyVar) = Map.foldMapWithKey
                                   (\var _ -> Option $ (\ratio -> Min $ Snd (var,ratio)) <$>
                                                           blandRatioDualRational var objective row)
                                   allVars
        (var,_) <- (getSnd . getMin) <$> mKeyVar
        return var

nextBasicDualWeight :: ( Ord k
                       , Ord a
                       , Num a
                       , Fractional a
                       ) => Equality k (Weight a)
                         -> IneqStdForm k a
                         -> Maybe k
nextBasicDualWeight (Equ objective) row =
  go 0
  where
    go n | all (\(Weight x) -> length x <= n) $ linExprVars objective = Nothing
         | otherwise = getFirst $
             First (nextBasicDualRational
                      (Equ $ linExprMapVars (Map.mapMaybe (\(Weight y) -> y !? n)) objective)
                      row)
          <> First (go $ n+1)


blandRatioDualRational :: ( Ord k
                          , Ord a
                          , Num a
                          , Fractional a
                          ) => k
                            -> Equality k a
                            -> IneqStdForm k a
                            -> Maybe a
blandRatioDualRational var (Equ objective) row = do
  let o = fromMaybe 0 $ Map.lookup var $ linExprVars objective
  x <- Map.lookup var $ ineqStdVars row
  guard $ x > 0
  return $ o / x


nextRowDual :: IntMap.IntMap (IneqStdForm k Rational) -> Maybe Int
nextRowDual xs = do
  let (Option mKeyVal) = IntMap.foldMapWithKey
                             (\slack row -> Option (Just (Min (Snd (slack,ineqStdConst row)))))
                             xs
  (slack,const) <- (getSnd . getMin) <$> mKeyVal
  guard $ const < 0
  return slack

-- * Equation Refactoring

-- | Orients / refactors an equation for one of its variables
flatten :: Ord k => k -> IneqStdForm k Rational -> IneqStdForm k Rational
flatten var row = case Map.lookup var $ ineqStdVars row of
  Just coeff -> ineqStdMapConst (/ coeff) $ fmap (/ coeff) row
  Nothing    -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
substituteRational :: Ord k =>
                      k
                   -> IneqStdForm k Rational -- ^ Replacement
                   -> IneqStdForm k Rational -- ^ Subject
                   -> IneqStdForm k Rational
substituteRational var replacement target =
  case Map.lookup var $ ineqStdVars target of
    Just existingCoeff -> let magnifiedReplacement =
                                fmap (existingCoeff *) $
                                ineqStdMapConst (existingCoeff *) replacement
                          in ineqStdMapVars  (`subMap` (ineqStdVars magnifiedReplacement)) $
                             ineqStdMapConst (subtract (ineqStdConst magnifiedReplacement)) target
    Nothing -> target

substituteWeight :: Ord k =>
                    k
                 -> IneqStdForm k Rational
                 -> IneqStdForm k (Weight Rational)
                 -> IneqStdForm k (Weight Rational)
substituteWeight var replacement target =
  case Map.lookup var $ ineqStdVars target of
    Just existingCoeff -> let magnifiedReplacement =
                                fmap (\c -> fmap (c *) existingCoeff) $
                                ineqStdMapConst ((compressWeight existingCoeff) *) replacement
                          in ineqStdMapVars  (`subMapWeight` (ineqStdVars magnifiedReplacement)) $
                             ineqStdMapConst (subtract (ineqStdConst magnifiedReplacement)) target
    Nothing -> target


-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimalWith :: ( Ord k
                   ) => (Equality k a -> Maybe k) -- ^ Next basic variable
                     -> (k -> IneqStdForm k Rational -> Equality k a -> Equality k a) -- ^ Substitution
                     -> (Tableau k k, Equality k a)
                     -> Maybe (Tableau k k, Equality k a)
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


pivotDualWith :: ( Ord k
                 ) => (Equality k a -> IneqStdForm k Rational -> Maybe k) -- ^ Next basic variable
                   -> (k -> IneqStdForm k Rational -> Equality k a -> Equality k a) -- ^ Substitution
                   -> (Tableau k k, Equality k a)
                   -> Maybe (Tableau k k, Equality k a)
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
simplexWith :: ((Tableau k0 k1, Equality k2 a) -> Maybe (Tableau k0 k1, Equality k2 a))
            -> (Tableau k0 k1, Equality k2 a)
            -> (Tableau k0 k1, Equality k2 a)
simplexWith piv x =
 case piv x of
   Just (cs,f) -> simplexWith piv (cs,f)
   Nothing -> x

-- | Primal maximizing optimization
simplexPrimalRational :: ( Ord k
                         ) => (Tableau k k, Equality k Rational)
                           -> (Tableau k k, Equality k Rational)
simplexPrimalRational = simplexWith (pivotPrimalWith nextBasicPrimalRational (\a b c -> unEquStd $ substituteRational a b $ EquStd c))

-- | Dual minimizing optimization
simplexDualRational :: ( Ord k
                       ) => (Tableau k k, Equality k Rational)
                         -> (Tableau k k, Equality k Rational)
simplexDualRational = simplexWith (pivotDualWith nextBasicDualRational (\a b c -> unEquStd $ substituteRational a b $ EquStd c))


