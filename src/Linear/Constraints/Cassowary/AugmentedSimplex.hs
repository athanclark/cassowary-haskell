{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , GADTs
  , TupleSections
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Prelude hiding (foldr, minimum, zip, lookup, empty, filter)

import Linear.Constraints.Tableau
import Linear.Constraints.Weights
import Linear.Grammar
import Linear.Class
import Data.Set.Class as Sets

import Data.List (elemIndex)
import Data.Maybe hiding (mapMaybe, catMaybes)
import Data.Monoid hiding ((<>))
import Data.Foldable
import Data.Witherable
import Data.Function (on)
import Data.Semigroup
import Data.Key
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Applicative hiding (empty)





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
nextBasicPrimal :: Equality Rational -> Maybe LinVarName
nextBasicPrimal (Equ xs _) = do
  let (Option mKeyVal) = foldMapWithKey
                             (\var coeff -> Option $ Just $ Min $ Snd (var,coeff))
                             (unLinVarMap xs)
  (var,coeff) <- (getSnd . getMin) <$> mKeyVal
  guard $ coeff < 0
  return var

-- | Finds the index of the next row to pivot on
nextRowPrimalRational :: LinVarName -> IntMap.IntMap (IneqStdForm Rational) -> Maybe Int
nextRowPrimalRational var xs = do
  let (Option mKeyVal) = foldMapWithKey
                             (\slack row -> Option $ (\ratio -> Min $ Snd (slack,ratio)) <$>
                                                         blandRatioPrimal var row)
                             xs
  (slack,ratio) <- (getSnd . getMin) <$> mKeyVal
  return slack


-- | Bland's method.
blandRatioPrimal :: LinVarName -> IneqStdForm Rational -> Maybe Rational
blandRatioPrimal var row = do
  coeff <- lookup var $ vars row
  guard $ coeff < 0
  return $ constVal row / coeff


-- ** Dual

nextBasicDual :: Equality Rational -> IneqStdForm Rational -> Maybe LinVarName
nextBasicDual objective row =
  let osMap = unLinVarMap $ vars objective
      xsMap = unLinVarMap $ vars row
      allVars = osMap `union` xsMap
  in do let (Option mKeyVar) = foldMapWithKey
                                   (\var _ -> Option $ (\ratio -> Min $ Snd (var,ratio)) <$>
                                                           blandRatioDual var objective row)
                                   allVars
        (var,_) <- (getSnd . getMin) <$> mKeyVar
        return var


blandRatioDual :: LinVarName -> Equality Rational -> IneqStdForm Rational -> Maybe Rational
blandRatioDual var objective row = do
  let o = fromMaybe 0 $ lookup var $ vars objective
  x <- lookup var $ vars row
  guard $ x > 0
  return $ o / x


nextRowDual :: IntMap.IntMap (IneqStdForm Rational) -> Maybe Int
nextRowDual xs = do
  let (Option mKeyVal) = foldMapWithKey
                             (\slack row -> Option (Just (Min (Snd (slack,constVal row)))))
                             xs
  (slack,const) <- (getSnd . getMin) <$> mKeyVal
  guard $ const < 0
  return slack

-- * Equation Refactoring

-- | Orients / refactors an equation for one of its variables
flatten :: LinVarName -> IneqStdForm Rational -> IneqStdForm Rational
flatten var row = case lookup var $ vars row of
  Just coeff -> mapConst (/ coeff) $ mapCoeffVals (/ coeff) row
  Nothing    -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
substituteRational :: LinVarName
                   -> IneqStdForm Rational -- ^ Replacement
                   -> IneqStdForm Rational -- ^ Subject
                   -> IneqStdForm Rational
substituteRational var replacement target =
  case lookup var (vars target) of
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
  case lookup var (vars target) of
    Just existingCoeff -> let magnifiedReplacement =
                                mapCoeffVals (\c -> fmap (c *) existingCoeff) $
                                mapConst ((compressWeight existingCoeff) *) replacement
                          in mapVars  (.-. vars magnifiedReplacement) $
                             mapConst (subtract (constVal magnifiedReplacement)) target
    Nothing -> target


-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimalRational :: (Tableau, Equality Rational) -> Maybe (Tableau, Equality Rational)
pivotPrimalRational (Tableau us us' basicc_s c_s, objective) = do
  var   <- nextBasicPrimal objective
  slack <- nextRowPrimalRational var c_s
  row   <- IntMap.lookup slack c_s
  let focal = flatten var row
      focal' = mapVars (delete var) focal
  return ( Tableau us us'
              (insertWith var focal' $
                  substituteRational var focal <$> basicc_s)
              (substituteRational var focal <$> delete slack c_s)
         , unEquStd $ substituteRational var focal (EquStd objective)
         )


pivotDualRational :: (Tableau, Equality Rational) -> Maybe (Tableau, Equality Rational)
pivotDualRational (Tableau us us' basicc_s c_s, objective) = do
  slack <- nextRowDual c_s
  row   <- IntMap.lookup slack c_s
  var   <- nextBasicDual objective row
  let replacement = flatten var row
      final       = mapVars (delete var) replacement
  return ( Tableau us us'
              (Map.insert var final $
                  substituteRational var replacement <$> basicc_s)
              (substituteRational var replacement <$> delete slack c_s)
         , unEquStd $ substituteRational var replacement (EquStd objective)
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
-- simplexPrimal :: ( Ord b
--                  , CanDivideTo b b b
--                  , CanDivideTo Rational b b
--                  , CanDivideTo Rational b Rational
--                  , CanMultiplyTo b b b
--                  , CanMultiplyTo Rational b b
--                  , CanMultiplyTo Rational b Rational
--                  , CanSubTo b b b
--                  , CanSubTo Rational b Rational
--                  , IsZero b
--                  , HasZero b
--                  ) => (Tableau b, Equality Rational) -> (Tableau b, Equality Rational)
-- simplexPrimal = simplexWith pivotPrimal

-- | Dual minimizing optimization
simplexDualRational :: (Tableau, Equality Rational) -> (Tableau, Equality Rational)
simplexDualRational = simplexWith pivotDualRational
