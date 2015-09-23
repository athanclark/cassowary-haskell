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
import Data.Monoid
import Data.Foldable
import Data.Witherable
import Data.Function (on)
import Data.Key
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Control.Monad
import Control.Applicative hiding (empty)


-- * Bland's Rule

-- ** Primal

-- | Most negative coefficient in objective function
nextBasicPrimal :: ( Ord b
                   , HasZero b
                   ) => Equality b -> Maybe LinVarName
nextBasicPrimal (Equ xs _) =
  let x = minimum xs
  in if x < zero'
     then fst <$> find (\y -> snd y == x) (Map.toList $ unLinVarMap xs)
     else Nothing

-- | Finds the index of the next row to pivot on
nextRowPrimal :: ( CanDivideTo Rational b b
                 , HasZero b
                 , HasConstant (a b)
                 , HasCoefficients a
                 , HasVariables a
                 , Ord b
                 , Eq (c (a b))
                 , Eq (c b)
                 , Functor c
                 , Foldable c
                 , Witherable c
                 , Monoid (c (a b))
                 , Monoid (c b)
                 ) => LinVarName -> c (a b) -> Maybe Int
nextRowPrimal col xs | xs == mempty = Nothing
                     | otherwise = smallest >> index
  where
    index :: Maybe Int
    index = elemIndex smallest $ toList $ fmap (blandRatioPrimal col) xs
    smallest = case mapMaybe (blandRatioPrimal col) xs of
      xs' | xs' == mempty -> Nothing
          | otherwise     -> Just $ minimum xs'


-- | Bland's method.
blandRatioPrimal :: ( CanDivideTo Rational b b
                    , HasConstant (a b)
                    , HasCoefficients a
                    , HasVariables a
                    , HasZero b
                    , Ord b
                    ) => LinVarName -> a b -> Maybe b
blandRatioPrimal col x = do
  coeff <- lookup col $ vars x
  if coeff < zero' then return $ constVal x ./. coeff
                   else Nothing
-- TODO: safe division - should be `[x] ./. [x] ~ [Maybe x]` - somehow figure out
-- a good way to order those, to find the minimum.
-- 0 ~ [] ~ [Nothing]


-- ** Dual

nextBasicDual :: ( Ord b
                 , CanDivideTo b b b
                 , HasZero b
                 , HasVariables a
                 ) => Equality b -> a b -> Maybe LinVarName
nextBasicDual o x =
  let osMap = unLinVarMap $ vars o
      xsMap = unLinVarMap $ vars x
      allVars = Map.keysSet osMap `union` Map.keysSet xsMap
  in do guard (size allVars > 0)
        ns <- nonEmpty $ catMaybes $ toList $
                Set.map (\col -> (,col) <$> blandRatioDual col o x) allVars
        let n = minimumBy (compare `on` fst) ns
        guard (fst n > zero')
        return $ snd n

  where nonEmpty :: Foldable f => f a -> Maybe (f a)
        nonEmpty = guardedBy (not . null)

        guardedBy :: Alternative f => (a -> Bool) -> a -> f a
        guardedBy p x = x <$ guard (p x)


blandRatioDual :: ( Ord b
                  , CanDivideTo b b b
                  , HasZero b
                  , HasVariables a
                  ) => LinVarName -> Equality b -> a b -> Maybe b
blandRatioDual col o x = do
  o' <- lookup col $ vars o
  x' <- lookup col $ vars x
  if x' > zero' then return $ o' ./. x'
                else Nothing


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
           , HasZero b
           , Eq b
           , CanDivideTo b b b
           , CanDivideTo Rational b Rational
           ) => LinVarName -> a b -> a b
flatten col x = case lookup col $ vars x of
  Just y  -> mapConst (./. y) $ mapCoeffVals (./. y) x
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"


-- | Replaces a separate equation @f@ for a variable @x@, in some target equation @g@ -
-- assuming @x = f@, and @1x ∈ f, and x ∈ g@.
-- @substitute var e1 e2@ really says "replace e1 for var in e2".
substitute :: ( Eq b
              , CanMultiplyTo b b b
              , CanMultiplyTo Rational b b
              , CanMultiplyTo Rational b Rational
              , CanSubTo b b b
              , CanSubTo Rational b Rational
              , HasZero b
              , HasConstant (a b)
              , HasConstant (a1 b)
              , HasCoefficients a
              , HasVariables a
              , HasVariables a1
              ) => LinVarName -> a b -> a1 b -> a1 b
substitute col replacement target =
  case lookup col $ vars target of
    Just coeff -> let replacement' = mapCoeffVals (.*. coeff)
                                   $ mapConst (.*. coeff) replacement
                  in mapVars  (.-. vars replacement')
                   $ mapConst (.-. constVal replacement') target
    Nothing -> target

-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimal :: ( Ord b
               , CanDivideTo b b b
               , CanDivideTo Rational b b
               , CanDivideTo Rational b Rational
               , CanMultiplyTo b b b
               , CanMultiplyTo Rational b b
               , CanMultiplyTo Rational b Rational
               , CanSubTo b b b
               , CanSubTo Rational b Rational
               , HasZero b
               ) => (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)
pivotPrimal (Tableau c_u (BNFTableau basicc_s, c_s) u, f) = do
  col      <- nextBasicPrimal f
  row      <- nextRowPrimal col c_s
  focalRaw <- lookup row c_s
  let focal = flatten col focalRaw
      focal' = mapVars (delete col) focal
  return ( Tableau c_u
              ( BNFTableau $ insertWith col focal' $
                  substitute col focal <$> basicc_s
              , substitute col focal <$> delete row c_s
              ) u
         , substitute col focal f
         )

-- | Performs a single dual pivot, minimizing the basic feasible solution.
pivotDual :: ( Ord b
             , CanDivideTo b b b
             , CanDivideTo Rational b Rational
             , CanMultiplyTo b b b
             , CanMultiplyTo Rational b b
             , CanMultiplyTo Rational b Rational
             , CanSubTo b b b
             , CanSubTo Rational b Rational
             , HasZero b
             ) => (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)
pivotDual (Tableau c_u (BNFTableau basicc_s, c_s) u, f) = do
  row      <- nextRowDual c_s
  focalRaw <- lookup row c_s
  col      <- nextBasicDual f focalRaw
  let focal = flatten col focalRaw
      focal' = mapVars (delete col) focal
  return ( Tableau c_u
              ( BNFTableau $ insertWith col focal' $
                  fmap (substitute col focal) basicc_s
              , substitute col focal <$> delete row c_s
              ) u
         , substitute col focal f
         )

-- | Optimize when given a pivot function
simplexWith :: (
               ) => ((Tableau b, Equality b) -> Maybe (Tableau b, Equality b))
                 -> (Tableau b, Equality b)
                 -> (Tableau b, Equality b)
simplexWith piv x =
 case piv x of
   Just (cs,f) -> simplexWith piv (cs,f)
   Nothing -> x

-- | Primal maximizing optimization
simplexPrimal :: ( Ord b
                 , CanDivideTo b b b
                 , CanDivideTo Rational b b
                 , CanDivideTo Rational b Rational
                 , CanMultiplyTo b b b
                 , CanMultiplyTo Rational b b
                 , CanMultiplyTo Rational b Rational
                 , CanSubTo b b b
                 , CanSubTo Rational b Rational
                 , HasZero b
                 ) => (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexPrimal = simplexWith pivotPrimal

-- | Primal maximizing optimization
simplexDual :: ( Ord b
               , CanDivideTo b b b
               , CanDivideTo Rational b Rational
               , CanMultiplyTo b b b
               , CanMultiplyTo Rational b b
               , CanMultiplyTo Rational b Rational
               , CanSubTo b b b
               , CanSubTo Rational b Rational
               , HasZero b
               ) => (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexDual = simplexWith pivotDual
