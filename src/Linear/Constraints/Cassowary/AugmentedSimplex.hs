{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  , GADTs
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Prelude hiding (foldr, minimum)

import Linear.Constraints.Tableau
import Linear.Constraints.Weights
import Linear.Grammar
import Linear.Class

import Data.List (elemIndex)
import Data.Maybe
import Data.Monoid
import Data.Foldable
import Data.Function (on)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.IntMap as IMap
import Control.Applicative


-- * Bland's Rule

-- | Most negative coefficient in objective function
nextBasicPrimal :: ( Ord b
                   , HasZero b
                   ) => Equality b -> Maybe LinVarName
nextBasicPrimal (Equ xs _) =
  let x = minimum $ Map.elems $ unLinVarMap xs
  in if x < zero' -- TODO
     then fst <$> find (\y -> snd y == x) (Map.toList $ unLinVarMap xs)
     else Nothing

-- | Finds the index of the next row to pivot on
nextRowPrimal :: ( CanDivideTo Rational b Rational
                 , HasConstant (a b)
                 , HasCoefficients a
                 , HasVariables a
                 , Eq (a b)
                 ) => LinVarName -> [a b] -> Maybe Int
nextRowPrimal col xs
  | xs == mempty = Nothing
  | otherwise = case smallest of
      Nothing -> Nothing
      Just s -> foldr (go $ Just s) Nothing $ fmap (blandRatioPrimal col) xs `zip` [0..]
  where
    go s (x,k) xs | s == x = Just k
                  | otherwise = xs
    smallest = case mapMaybe (blandRatioPrimal col) xs of
      xs' | xs' == mempty -> Nothing
          | otherwise -> Just $ minimum xs'

-- TODO: weights might break concept of rationals
-- | Using Bland's method.
blandRatioPrimal :: ( HasConstant (a b)
                    , HasCoefficients a
                    , HasVariables a
                    , CanDivideTo Rational b Rational
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
-- flatten :: ( HasCoefficients a b
--            , HasConstant a
--            , HasVariables a b
--            , Num b
--            ) => LinVarName -> a -> a
flatten col x = case Map.lookup col $ unLinVarMap $ vars x of
  Just y  -> mapConst (./. y) $ mapCoeffVals (./. y) x
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"

-- substitute :: ( HasVariables a1
--               , HasVariables a2
--               , HasCoefficients a2
--               , HasConstant (a1 b4)
--               , HasConstant (a2 b4)
--               , CanMultiplyTo b1 b2 b3
--               , CanMultiplyTo b3 Rational b4
--               , CanAddTo b4 b4 b4
--               , b2 ~ b4
--               ) => LinVarName -> a1 b1 -> a2 b2 -> a2 b4
substitute col focal target =
  case Map.lookup col $ unLinVarMap $ vars target of -- TODO: make right-biased mult between two Weights
    Just coeff -> let focal' = mapCoeffVals (\x -> x .*. coeff .*. (-1 :: Rational)) focal
                      go (LinVarMap xs) = let xs' = Map.unionWith (.+.) xs (unLinVarMap $ vars focal')
                                          in LinVarMap $ Map.filter (/= mempty) xs'
                  in mapConst (\x -> x .-. (constVal focal' .*. coeff)) $ mapVars go target
    Nothing -> target


-- | Performs a single pivot
pivotPrimal :: (Tableau b, Equality b) -> Maybe (Tableau b, Equality b)
pivotPrimal (Tableau c_u (BNFTableau basicc_s, c_s) u, f) =
  let mCol = nextBasicPrimal f
      mRow = mCol >>= (`nextRowPrimal` c_s)
  in case (mCol, mRow) of
       (Just col, Just row) ->
          let focal = flatten col $ c_s !! row
              focal' = mapVars (\(LinVarMap xs) -> LinVarMap $ Map.delete col xs) focal
          in Just ( Tableau c_u
                      ( BNFTableau $ Map.insert col focal' $
                          fmap (substitute col focal) basicc_s
                      , substitute col focal <$> take row c_s ++ drop (row+1) c_s
                      ) u
                  , unEquStd $ substitute col focal $ EquStd f
                  )
       _ -> Nothing


-- | Simplex optimization
simplexPrimal :: (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexPrimal x =
  case pivotPrimal x of
    Just (cs,f) -> simplexPrimal (cs,f)
    Nothing -> x

simplexDual :: (Tableau b, Equality b) -> (Tableau b, Equality b)
simplexDual xs = let (xs, revert) = transposeTab xs
  in untransposeTab (simplexPrimal xs, revert)

-- only need to transpose restricted vars, as simplex only acts on those.
transposeTab :: (Tableau b, Equality b) -> ((Tableau b, Equality b), Map.Map Integer String)
transposeTab (Tableau us (BNFTableau sus,ss) u, f) =
  let constrVars = Set.unions $ IMap.elems $ fmap (Map.keysSet . unLinVarMap . vars) ss
      basicVars = Map.keysSet sus
      basicBodyVars = Set.unions $ map (Map.keysSet . unLinVarMap . vars) $ Map.elems sus
      allVars = Set.unions [ constrVars
                           , basicVars
                           , basicBodyVars
                           ] -- excludes objective solution variable
      allVarsMap = Set.toList allVars `zip` [0..]
      vertToHoriz v (cs,newslack) = if v `Set.member` basicVars
        then case Map.lookup v sus of
               Just ex -> maybe
                  (error "`transposeTab` called to expressions without slack variables.")
                  (\oldslack ->
                      ( let ex' = EquStd $ Equ (LinVarMap $ Map.fromList
                                    [ (VarMain $ unLinVarName $ varName oldslack, 1)
                                    , (VarSlack newslack, 1)
                                    ]) 0
                        in IMap.insert (fromIntegral $ unVarSlack $ varName oldslack) ex' cs
                      , newslack - 1
                      )
                  ) $ findSlack ex
        else undefined -- find v in each equation, coeff as new main var coeff

      findSlack ex = find isSlack $ map (uncurry LinVar) $ Map.toList $ unLinVarMap $ vars ex

      isSlack (LinVar (VarSlack _) _) = True
      isSlack _ = False
  in ((Tableau us (mempty, fst $ foldr vertToHoriz (mempty,fromIntegral $ Set.size allVars) allVars) u, f), undefined)

untransposeTab :: ((Tableau b, Equality b), Map.Map Integer String) -> (Tableau b, Equality b)
untransposeTab = undefined
