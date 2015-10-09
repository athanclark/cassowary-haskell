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
import Linear.Class

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
nextBasicPrimalRational :: Equality Rational -> Maybe LinVarName
nextBasicPrimalRational (Equ xs _) = do
  let (Option mKeyVal) = Map.foldMapWithKey
                             (\var coeff -> Option $ Just $ Min $ Snd (var,coeff))
                             (unLinVarMap xs)
  (var,coeff) <- (getSnd . getMin) <$> mKeyVal
  guard $ coeff < 0
  return var

nextBasicPrimalWeight :: Equality (Weight Rational) -> Maybe LinVarName
nextBasicPrimalWeight (Equ (LinVarMap xs) xc) = do
  go 0
  where
    go n | all (\(Weight x) -> length x <= n) xs = Nothing
         | otherwise = getFirst $
             First (nextBasicPrimalRational $
               Equ (LinVarMap $ Map.mapMaybe (\(Weight y) -> y !? n) xs) xc)
          <> First (go $ n+1)



-- | Finds the index of the next row to pivot on
nextRowPrimal :: LinVarName -> IntMap.IntMap (IneqStdForm Rational) -> Maybe Int
nextRowPrimal var xs = do
  let (Option mKeyVal) = IntMap.foldMapWithKey
                             (\slack row -> Option $ (\ratio -> Min $ Snd (slack,ratio)) <$>
                                                         blandRatioPrimal var row)
                             xs
  (slack,_) <- (getSnd . getMin) <$> mKeyVal
  return slack


-- | Bland's method.
blandRatioPrimal :: LinVarName -> IneqStdForm Rational -> Maybe Rational
blandRatioPrimal var row = do
  coeff <- Map.lookup var $ unLinVarMap $ vars row
  guard $ coeff < 0
  return $ constVal row / coeff


-- ** Dual

nextBasicDualRational :: Equality Rational -> IneqStdForm Rational -> Maybe LinVarName
nextBasicDualRational objective row =
  let osMap = unLinVarMap $ vars objective
      xsMap = unLinVarMap $ vars row
      allVars = osMap <> xsMap
  in do let (Option mKeyVar) = Map.foldMapWithKey
                                   (\var _ -> Option $ (\ratio -> Min $ Snd (var,ratio)) <$>
                                                           blandRatioDualRational var objective row)
                                   allVars
        (var,_) <- (getSnd . getMin) <$> mKeyVar
        return var

nextBasicDualWeight :: Equality (Weight Rational) -> IneqStdForm Rational -> Maybe LinVarName
nextBasicDualWeight (Equ (LinVarMap xs) xc) row =
  go 0
  where
    go n | all (\(Weight x) -> length x <= n) xs = Nothing
         | otherwise = getFirst $
             First (nextBasicDualRational
                      (Equ (LinVarMap $ Map.mapMaybe (\(Weight y) ->
                                                          y !? n) xs) xc)
                      row)
          <> First (go $ n+1)


blandRatioDualRational :: LinVarName -> Equality Rational -> IneqStdForm Rational -> Maybe Rational
blandRatioDualRational var objective row = do
  let o = fromMaybe 0 $ Map.lookup var $ unLinVarMap $ vars objective
  x <- Map.lookup var $ unLinVarMap $ vars row
  guard $ x > 0
  return $ o / x


nextRowDual :: IntMap.IntMap (IneqStdForm Rational) -> Maybe Int
nextRowDual xs = do
  let (Option mKeyVal) = IntMap.foldMapWithKey
                             (\slack row -> Option (Just (Min (Snd (slack,constVal row)))))
                             xs
  (slack,const) <- (getSnd . getMin) <$> mKeyVal
  guard $ const < 0
  return slack

-- * Equation Refactoring

-- | Orients / refactors an equation for one of its variables
flatten :: LinVarName -> IneqStdForm Rational -> IneqStdForm Rational
flatten var row = case Map.lookup var $ unLinVarMap $ vars row of
  Just coeff -> mapConst (/ coeff) $ mapCoeffVals (/ coeff) row
  Nothing    -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
substituteRational :: LinVarName
                   -> IneqStdForm Rational -- ^ Replacement
                   -> IneqStdForm Rational -- ^ Subject
                   -> IneqStdForm Rational
substituteRational var replacement target =
  case Map.lookup var $ unLinVarMap $ vars target of
    Just existingCoeff -> let magnifiedReplacement =
                                mapCoeffVals (existingCoeff *) $
                                mapConst (existingCoeff *) replacement
                          in mapVars  (.-. vars magnifiedReplacement) $
                             mapConst (subtract (constVal magnifiedReplacement)) target
    Nothing -> target

substituteWeight :: LinVarName
                 -> IneqStdForm Rational
                 -> IneqStdForm (Weight Rational)
                 -> IneqStdForm (Weight Rational)
substituteWeight var replacement target =
  case Map.lookup var $ unLinVarMap $ vars target of
    Just existingCoeff -> let magnifiedReplacement =
                                mapCoeffVals (\c -> fmap (c *) existingCoeff) $
                                mapConst ((compressWeight existingCoeff) *) replacement
                          in mapVars  (.-. vars magnifiedReplacement) $
                             mapConst (subtract (constVal magnifiedReplacement)) target
    Nothing -> target


-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimalWith :: (Equality a -> Maybe LinVarName) -- ^ Next basic variable
                -> (LinVarName -> IneqStdForm Rational -> Equality a -> Equality a) -- ^ Substitution
                -> (Tableau, Equality a)
                -> Maybe (Tableau, Equality a)
pivotPrimalWith nextBasic substituteObj (Tableau us us' basicc_s c_s, objective) = do
  var   <- nextBasic objective
  slack <- nextRowPrimal var c_s
  row   <- IntMap.lookup slack c_s
  let focal = flatten var row
      focal' = mapVars (\(LinVarMap xs) -> LinVarMap $ Map.delete var xs) focal
  return ( Tableau us us'
              (Map.insert var focal' $
                  substituteRational var focal <$> basicc_s)
              (substituteRational var focal <$> IntMap.delete slack c_s)
         , substituteObj var focal objective
         )


pivotDualWith :: (Equality a -> IneqStdForm Rational -> Maybe LinVarName) -- ^ Next basic variable
              -> (LinVarName -> IneqStdForm Rational -> Equality a -> Equality a) -- ^ Substitution
              -> (Tableau, Equality a)
              -> Maybe (Tableau, Equality a)
pivotDualWith nextBasic substituteObj (Tableau us us' basicc_s c_s, objective) = do
  slack <- nextRowDual c_s
  row   <- IntMap.lookup slack c_s
  var   <- nextBasic objective row
  let replacement = flatten var row
      final       = mapVars (\(LinVarMap xs) -> LinVarMap $ Map.delete var xs) replacement
  return ( Tableau us us'
              (Map.insert var final $
                  substituteRational var replacement <$> basicc_s)
              (substituteRational var replacement <$> IntMap.delete slack c_s)
         , substituteObj var replacement objective
         )

-- | Optimize when given a pivot function
simplexWith :: (
               ) => ((Tableau, Equality Rational) -> Maybe (Tableau, Equality Rational))
                 -> (Tableau, Equality Rational)
                 -> (Tableau, Equality Rational)
simplexWith piv x =
 case piv x of
   Just (cs,f) -> simplexWith piv (cs,f)
   Nothing -> x

-- | Primal maximizing optimization
simplexPrimalRational :: (Tableau, Equality Rational) -> (Tableau, Equality Rational)
simplexPrimalRational = simplexWith (pivotPrimalWith nextBasicPrimalRational (\a b c -> unEquStd $ substituteRational a b $ EquStd c))

-- | Dual minimizing optimization
simplexDualRational :: (Tableau, Equality Rational) -> (Tableau, Equality Rational)
simplexDualRational = simplexWith (pivotDualWith nextBasicDualRational (\a b c -> unEquStd $ substituteRational a b $ EquStd c))


