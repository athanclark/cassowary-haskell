{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , GADTs
  , TupleSections
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Prelude hiding (foldr, minimum, zip)

import Linear.Constraints.Tableau
import Linear.Constraints.Weights
import Linear.Grammar
import Linear.Class

import Data.List (elemIndex)
import Data.Maybe hiding (mapMaybe)
import Data.Monoid
import Data.Foldable
import Data.Witherable
import Data.Function (on)
import Data.STRef
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IntMap
import Control.Applicative
import Control.Monad.ST


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
  coeff <- Map.lookup col (unLinVarMap $ vars x)
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
      allVars = Map.keysSet osMap <> Map.keysSet xsMap
  in if Set.size allVars > 0
     then let n = minimumBy (compare `on` fst) $ trim $
                    Set.map (\col -> (,col) <$> blandRatioDual col o x) allVars
          in if fst n > zero' then return $ snd n
                              else Nothing
     else Nothing
  where
    trim = foldr go Set.empty
      where
        go Nothing acc = acc
        go (Just a) acc = Set.insert a acc


blandRatioDual :: ( Ord b
                  , CanDivideTo b b b
                  , HasZero b
                  , HasVariables a
                  ) => LinVarName -> Equality b -> a b -> Maybe b
blandRatioDual col o x = do
  o' <- Map.lookup col (unLinVarMap $ vars o)
  x' <- Map.lookup col (unLinVarMap $ vars x)
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

-- | Orients equation over some (existing) variable
flatten :: ( HasCoefficients a
           , HasVariables a
           , HasConstant (a b)
           , CanDivideTo b b b
           , CanDivideTo Rational b Rational
           ) => LinVarName -> a b -> a b
flatten col x = case Map.lookup col $ unLinVarMap $ vars x of
  Just y  -> mapConst (./. y) $ mapCoeffVals (./. y) x
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"


substitute :: ( Eq b
              , CanMultiplyTo b b b
              , CanMultiplyTo Rational b b
              , CanSubTo b b b
              , CanSubTo Rational b Rational
              , HasZero b
              , HasConstant (a b)
              , HasConstant (a1 b)
              , HasCoefficients a
              , HasVariables a
              , HasVariables a1
              ) => LinVarName -> a b -> a1 b -> a1 b
substitute col focal target =
  case Map.lookup col $ unLinVarMap $ vars target of
    Just coeff -> let focal' = mapCoeffVals (.*. coeff) focal
                  in mapConst (.-. (constVal focal' .*. coeff)) $
                     mapVars  (.-. vars focal') target
    Nothing -> target

-- * Pivots

-- | Performs a single primal pivot, maximizing the basic feasible solution.
pivotPrimal :: ( Ord b
               , CanDivideTo b b b
               , CanDivideTo Rational b b
               , CanDivideTo Rational b Rational
               , CanMultiplyTo b b b
               , CanMultiplyTo Rational b b
               , CanSubTo b b b
               , CanSubTo Rational b Rational
               , HasZero b
               ) => (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)
pivotPrimal (Tableau c_u (BNFTableau basicc_s, c_s) u, f) = do
  col      <- nextBasicPrimal f
  row      <- nextRowPrimal col c_s
  focalRaw <- IntMap.lookup row c_s
  let focal = flatten col focalRaw
      focal' = mapVars (LinVarMap . Map.delete col . unLinVarMap) focal
  return ( Tableau c_u
              ( BNFTableau $ Map.insert col focal' $
                  fmap (substitute col focal) basicc_s
              , substitute col focal <$> IntMap.delete row c_s
              ) u
         , substitute col focal f
         )

-- | Performs a single dual pivot, minimizing the basic feasible solution.
pivotDual :: ( Ord b
             , CanDivideTo b b b
             , CanDivideTo Rational b Rational
             , CanMultiplyTo b b b
             , CanMultiplyTo Rational b b
             , CanSubTo b b b
             , CanSubTo Rational b Rational
             , HasZero b
             ) => (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)
pivotDual (Tableau c_u (BNFTableau basicc_s, c_s) u, f) = do
  row      <- nextRowDual c_s
  focalRaw <- IntMap.lookup row c_s
  col      <- nextBasicDual f focalRaw
  let focal = flatten col focalRaw
      focal' = mapVars (LinVarMap . Map.delete col . unLinVarMap) focal
  return ( Tableau c_u
              ( BNFTableau $ Map.insert col focal' $
                  fmap (substitute col focal) basicc_s
              , substitute col focal <$> IntMap.delete row c_s
              ) u
         , substitute col focal f
         )

-- | Primal maximizing optimization
simplexPrimal :: ( Ord b
                 , CanDivideTo b b b
                 , CanDivideTo Rational b b
                 , CanDivideTo Rational b Rational
                 , CanMultiplyTo b b b
                 , CanMultiplyTo Rational b b
                 , CanSubTo b b b
                 , CanSubTo Rational b Rational
                 , HasZero b
                 ) => (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexPrimal x =
  case pivotPrimal x of
    Just (cs,f) -> simplexPrimal (cs,f)
    Nothing -> x

-- | Dual minimizing optimization
simplexDual :: (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexDual xs = undefined
