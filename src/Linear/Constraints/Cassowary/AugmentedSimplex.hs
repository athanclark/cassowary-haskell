{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , GADTs
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
nextRowPrimal :: ( CanDivideTo Rational b Rational
                 , HasConstant (a b)
                 , HasCoefficients a
                 , HasVariables a
                 , Eq (c (a b))
                 , Eq (c Rational)
                 , Functor c
                 , Foldable c
                 , Witherable c
                 , Monoid (c (a b))
                 , Monoid (c Rational)
                 ) => LinVarName -> c (a b) -> Maybe Int
nextRowPrimal col xs | xs == mempty = Nothing
                     | otherwise = case smallest of
    Nothing -> Nothing
    _ -> runST $ do i <- newSTRef 0
                    foldlM (go i) Nothing $ fmap (blandRatioPrimal col) xs
  where
    smallest :: Maybe Rational
    smallest = case mapMaybe (blandRatioPrimal col) xs of
      xs' | xs' == mempty -> Nothing
          | otherwise     -> Just $ minimum xs'
    go :: STRef s Int -> Maybe Int -> Maybe Rational -> ST s (Maybe Int)
    go i acc x | smallest == x = do k <- readSTRef i
                                    writeSTRef i $ k+1
                                    return $ Just k
               | otherwise = return acc

-- | Bland's method.
blandRatioPrimal :: ( CanDivideTo Rational b Rational
                    , HasConstant (a b)
                    , HasCoefficients a
                    , HasVariables a
                    ) => LinVarName -> a b -> Maybe Rational
blandRatioPrimal col x =
  let vs = vars x
  in  Map.lookup col (unLinVarMap vs) >>=
        \coeff -> Just $ constVal x ./. coeff

nextBasicDual :: ( Num b
                 , Eq b
                 , Ord b
                 , Fractional b
                 ) => Equality b -> IneqStdForm b -> Maybe Int
nextBasicDual obj xs = undefined


nextRowDual :: ( Ord b
               , Eq (a b)
               , HasConstant (a b)
               ) => [a b] -> Maybe Int
nextRowDual xs =
  let x = minimumBy (compare `on` constVal) xs
  in if constVal x < 0
     then elemIndex x xs
     else Nothing

-- | Orients equation over some (existing) variable
flatten :: ( HasCoefficients a
           , HasVariables a
           , HasConstant (a b)
           , CanDivideTo Rational b Rational
           , CanDivideTo b b b
           ) => LinVarName -> a b -> a b
flatten col x = case Map.lookup col $ unLinVarMap $ vars x of
  Just y  -> mapConst (./. y) $ mapCoeffVals (./. y) x
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"

substitute :: ( Eq b2
              , CanMultiplyTo b2 b b2
              , CanMultiplyTo b2 Rational b
              , CanMultiplyTo Rational b2 b1
              , CanSubTo Rational b1 Rational
              , HasZero b2
              , CanAddTo b2 b2 b2
              , HasConstant (a b2)
              , HasConstant (a1 b2)
              , HasCoefficients a
              , HasVariables a
              , HasVariables a1
              ) => LinVarName -> a b2 -> a1 b2 -> a1 b2
substitute col focal target =
  case Map.lookup col $ unLinVarMap $ vars target of -- TODO: make right-biased mult between two Weights
    Just coeff -> let focal' = mapCoeffVals (\x -> x .*. coeff .*. (-1 :: Rational)) focal
                      go (LinVarMap xs) = let xs' = Map.unionWith (.+.) xs (unLinVarMap $ vars focal')
                                          in LinVarMap $ Map.filter (/= zero') xs'
                  in mapConst (\x -> x .-. (constVal focal' .*. coeff)) $ mapVars go target
    Nothing -> target


-- | Performs a single pivot
pivotPrimal :: ( Ord b
               , CanDivideTo b b b
               , CanDivideTo Rational b Rational
               , CanMultiplyTo b b1 b
               , CanMultiplyTo b Rational b1
               , CanMultiplyTo Rational b b2
               , CanSubTo Rational b2 Rational
               , HasZero b
               , CanAddTo b b b
               ) => (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)
pivotPrimal (Tableau c_u (BNFTableau basicc_s, c_s) u, f) =
  let mCol = nextBasicPrimal f
      mRow = mCol >>= (`nextRowPrimal` c_s)
  in case (mCol, mRow) of
        (Just col, Just row) ->
          let focal = flatten col $ fromJust $ IntMap.lookup row c_s
              focal' = mapVars (LinVarMap . Map.delete col . unLinVarMap) focal
          in Just ( Tableau c_u
                      ( BNFTableau $ Map.insert col focal' $
                          fmap (substitute col focal) basicc_s
                      , substitute col focal <$> IntMap.delete row c_s
                      ) u
                  , unEquStd $ substitute col focal $ EquStd f
                  )
        _ -> Nothing


-- | Simplex optimization
simplexPrimal :: ( Ord b
                 , CanDivideTo b b b
                 , CanDivideTo Rational b Rational
                 , CanMultiplyTo b b1 b
                 , CanMultiplyTo b Rational b1
                 , CanMultiplyTo Rational b b2
                 , CanSubTo Rational b2 Rational
                 , HasZero b
                 , CanAddTo b b b
                 ) => (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexPrimal x =
  case pivotPrimal x of
    Just (cs,f) -> simplexPrimal (cs,f)
    Nothing -> x

simplexDual :: (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexDual xs = undefined
