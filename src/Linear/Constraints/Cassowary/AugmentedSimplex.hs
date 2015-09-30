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

-- | Most negative coefficient in objective function
-- nextBasicPrimal :: Equality b -> Maybe LinVarName
nextBasicPrimal (Equ xs _) = do
  guard $ size xs > 0
  let x = minimum xs
  guard $ x < zero'
  (k,_) <- find (\(_,v) -> v == x) (Map.toList $ unLinVarMap xs)
  return k

newtype Snd a b = Snd {getSnd :: (a,b)}

instance Eq b => Eq (Snd a b) where
  (Snd (_,y1)) == (Snd (_,y2)) = y1 == y2

instance (Ord b) => Ord (Snd a b) where
  (Snd (_,y1)) `compare` (Snd (_,y2)) = compare y1 y2


-- | Finds the index of the next row to pivot on
nextRowPrimalRational :: LinVarName -> IntMap.IntMap (IneqStdForm Rational) -> Maybe Int
nextRowPrimalRational col xs = do
  let (Option mKeyVal) = foldMapWithKey
                             (\k v -> Option $ (\r -> Min $ Snd (k,r)) <$> blandRatioPrimal col v)
                             xs
  (k,v) <- (getSnd . getMin) <$> mKeyVal
  guard $ v < 0
  return k


-- | Bland's method.
blandRatioPrimal :: LinVarName -> IneqStdForm Rational -> Maybe Rational
blandRatioPrimal col x = do
  coeff <- lookup col $ vars x
  guard $ coeff < 0
  return $ constVal x / coeff


-- ** Dual

newtype Fst a b = Fst {getFst :: (a,b)}

instance Ord a => Semigroup (Min (Fst a b)) where
  (Min (Fst (x1,y1))) <> (Min (Fst (x2,y2))) | x1 < x2   = Min $ Fst (x1,y1)
                                             | otherwise = Min $ Fst (x2,y2)

-- nextBasicDual :: ( Ord b
--                  , CanDivideTo b b b
--                  , HasZero b
--                  , HasVariables a
--                  ) => Equality b -> a b -> Maybe LinVarName
nextBasicDual o x =
  let osMap = unLinVarMap $ vars o
      xsMap = unLinVarMap $ vars x
      allVars = osMap `union` xsMap
  in do let (Option mKeyVar) = foldMapWithKey
                                   (\col _ -> Option ((\r -> Min $ Snd (col,r)) <$> blandRatioDual col o x))
                                   allVars
        (k,v) <- (getSnd . getMin) <$> mKeyVar
        guard (v > zero')
        return k


blandRatioDual :: ( Ord b
                  , CanDivideTo b b b
                  , HasZero b
                  , HasVariables a
                  ) => LinVarName -> Equality b -> a b -> Maybe b
blandRatioDual col o x = do
  o' <- lookup col $ vars o
  x' <- lookup col $ vars x
  guard $ x' > zero'
  return $ o' ./. x'


nextRowDual :: ( Eq (a b)
               , HasConstant (a b)
               , Foldable c
               ) => c (a b) -> Maybe Int
nextRowDual xs =
  let x = minimumBy (compare `on` constVal) xs
  in  if constVal x < zero'
      then elemIndex x $ toList xs
      else Nothing

-- * Equation Refactoring

-- | Orients / refactors an equation for one of its variables
flatten :: ( HasCoefficients a
           , HasVariables a
           , HasConstant (a b)
           , CanDivideTo b b b
           , CanDivideTo Rational b Rational
           ) => LinVarName -> a b -> a b
flatten col x = case lookup col $ vars x of
  Just y  -> mapConst (./. y) $ mapCoeffVals (./. y) x
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
-- assuming @x = f@, and @1x ∈ f, and x ∈ g@.
-- @substitute var e1 e2@ really says "replace e1 for var in e2".
-- substitute :: ( CanMultiplyTo b0 b1 b1
--               , CanMultiplyTo Rational b1 b1
--               , CanMultiplyTo Rational b0 Rational
--               , CanSubTo b0 b0 b0
--               , CanSubTo Rational b0 Rational
--               , IsZero b0
--               , HasConstant (a0 b)
--               , HasConstant (a1 b)
--               , HasCoefficients a0
--               , HasVariables a0
--               , HasVariables a1
--               ) => LinVarName -> a0 b0 -> a1 b1 -> a1 b1
substitute col replacement target =
  case lookup col $ vars target of
    Just existing -> let replacement' = mapCoeffVals (.*. existing)
                                      $ mapConst (.*. existing) replacement
                     in mapVars  (.-. vars replacement')
                     $ mapConst (.-. constVal replacement') target
    Nothing       -> target

-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
-- pivotPrimal :: ( Ord b
--                , CanDivideTo b b b
--                , CanDivideTo Rational b b
--                , CanDivideTo Rational b Rational
--                , CanMultiplyTo b b b
--                , CanMultiplyTo Rational b b
--                , CanMultiplyTo Rational b Rational
--                , CanSubTo b b b
--                , CanSubTo Rational b Rational
--                , IsZero b
--                , HasZero b
--                ) => (Tableau b, Equality Rational) -> Maybe (Tableau b, Equality b)
-- pivotPrimal (Tableau c_u (BNFTableau basicc_s, c_s), f) = do
--   col      <- nextBasicPrimal f
--   row      <- nextRowPrimal col c_s
--   focalRaw <- lookup row c_s
--   let focal = flatten col focalRaw
--       focal' = mapVars (delete col) focal
--   return ( Tableau c_u
--               ( BNFTableau $ insertWith col focal' $
--                   substitute col focal <$> basicc_s
--               , substitute col focal <$> delete row c_s
--               )
--          , substitute col focal f
--          )


class PivotDual b where
  pivotDual :: (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)


instance PivotDual Rational where
  pivotDual (Tableau c_u (BNFTableau basicc_s, c_s), f) = do
    row      <- nextRowDual c_s
    focalRaw <- lookup row c_s
    col      <- nextBasicDual f focalRaw
    let focal = flatten col focalRaw
        focal' = mapVars (delete col) focal
    return ( Tableau c_u
                ( BNFTableau $ insertWith col focal' $
                    substitute col focal <$> basicc_s
                , substitute col focal <$> delete row c_s
                )
           , substitute col focal f
           )

-- | Optimize when given a pivot function
simplexWith :: (
               ) => ((Tableau b, Equality Rational) -> Maybe (Tableau b, Equality Rational))
                 -> (Tableau b, Equality Rational)
                 -> (Tableau b, Equality Rational)
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

-- | Primal maximizing optimization
-- simplexDual :: ( Ord b
--                , CanDivideTo b b b
--                , CanDivideTo Rational b Rational
--                , CanMultiplyTo b b b
--                , CanMultiplyTo Rational b b
--                , CanMultiplyTo Rational b Rational
--                , CanSubTo b b b
--                , CanSubTo Rational b Rational
--                , IsZero b
--                , HasZero b
--                ) => (Tableau b, Equality Rational) -> (Tableau b, Equality Rational)
simplexDual = simplexWith pivotDual
