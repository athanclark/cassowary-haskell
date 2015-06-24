{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Prelude hiding (foldr, minimum)

import Linear.Constraints.Class
import Linear.Constraints.Tableau
import Linear.Grammar

import Data.List (elemIndex)
import Data.Maybe
import Data.Monoid
import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set


-- * Bland's Rule

-- | Most negative coefficient in objective function
nextBasic :: Equality -> Maybe LinVarName
nextBasic (Equ xs _) =
  let x = minimumBy (\(_,v) (_,v') -> compare v v') $ Map.toList $ unLinVarMap xs
  in if snd x < 0
     then Just $ fst x
     else Nothing

-- | Finds the index of the next row to pivot on - note, list must have
nextRow :: ( HasConstant a
           , HasMainVars a
           ) => LinVarName -> [a] -> Maybe Int
nextRow _ [] = Nothing
nextRow col xs = case smallest of
  Nothing -> Nothing
  Just s -> elemIndex (Just s) $ map (blandRatio col) xs
  where
    smallest = case mapMaybe (blandRatio col) xs of
      [] -> Nothing
      xs' -> Just $ minimum xs'

-- | Using Bland's method.
blandRatio :: ( HasConstant a
              , HasMainVars a
              ) => LinVarName -> a -> Maybe Rational
blandRatio col x = Map.lookup col (unLinVarMap $ mainVars x) >>=
  \coeff -> Just $ constVal x / coeff

-- | Orients equation over some (existing) variable
flatten :: ( HasCoefficients a
           , HasConstant a
           , HasMainVars a
           ) => LinVarName -> a -> a
flatten col x = case Map.lookup col $ unLinVarMap $ mainVars x of
  Just y -> mapConst (/ y) $ mapCoeffs (map (/ y)) x
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"

substitute :: ( HasMainVars a
              , HasConstant a
              , HasCoefficients a
              ) => LinVarName -> a -> a -> a
substitute col focal target =
  case Map.lookup col $ unLinVarMap $ mainVars target of
    Just coeff -> let focal' = mapCoeffs (map (\x -> x * coeff * (-1))) focal
                      go (LinVarMap xs) = let xs' = Map.unionWith (+) xs (unLinVarMap $ mainVars focal')
                                          in LinVarMap $ Map.filter (/= 0) xs'
                  in mapConst (\x -> x - constVal focal' * coeff) $ mapMainVars go target
    Nothing -> target


-- | Performs a single pivot
pivot :: (Tableau, Equality) -> Maybe (Tableau, Equality)
pivot (Tableau c_u (BNFTableau basicc_s, c_s) u, f) =
  let mCol = nextBasic f
      mRow = mCol >>= (`nextRow` c_s)
  in case (mCol, mRow) of
       (Just col, Just row) -> let csPre = take row c_s
                                   csPost = drop (row+1) c_s
                                   focal = flatten col $ c_s !! row
                                   focal' = mapMainVars (\(LinVarMap xs) ->
                                              LinVarMap $ Map.delete col xs) focal
          in Just ( Tableau c_u
                      ( BNFTableau $ Map.insert col focal' $
                          fmap (substitute col focal) basicc_s
                      , map (substitute col focal) csPre
                     ++ map (substitute col focal) csPost
                      ) u
                  , unEquStd $ substitute col focal $ EquStd f
                  )
       _ -> Nothing


-- | Simplex optimization
simplexPrimal :: (Tableau, Equality) -> (Tableau, Equality)
simplexPrimal x =
  case pivot x of
    Just (cs,f) -> simplexPrimal (cs,f)
    Nothing -> x

simplexDual :: (Tableau, Equality) -> (Tableau, Equality)
simplexDual xs = let (xs, revert) = transposeTab xs
  in untransposeTab (simplexPrimal xs, revert)

-- only need to transpose restricted vars, as simplex only acts on those.
transposeTab :: (Tableau, Equality) -> ((Tableau, Equality), Map.Map Integer String)
transposeTab (Tableau us (BNFTableau sus,ss) u, f) =
  let constrVars = Set.unions $ map (Map.keysSet . unLinVarMap . mainVars) ss
      basicVars = Map.keysSet sus
      basicBodyVars = Set.unions $ map (Map.keysSet . unLinVarMap . mainVars) $ Map.elems sus
      allVars = Set.unions [ constrVars
                           , basicVars
                           , basicBodyVars
                           ] -- excludes objective solution variable
      allVarsMap = Set.toList allVars `zip` [0..]
      go v (cs,newslack) = if v `Set.member` basicVars
        then case Map.lookup v sus of
               Just ex -> maybe
                  (error "`transposeTab` called to expressions without slack variables.")
                  (\oldslack ->
                      ( EquStd (Equ (LinVarMap $ Map.fromList
                          [ (VarMain $ unLinVarName $ varName oldslack, 1)
                          , (VarSlack newslack, 1)
                          ]) 0) : cs
                      , newslack - 1
                      )
                  ) $ findSlack ex
        else undefined -- find v in each equation, coeff as new main var coeff

      findSlack ex = find isSlack $ map (uncurry LinVar) $ Map.toList $ unLinVarMap $ mainVars ex

      isSlack (LinVar (VarSlack _) _) = True
      isSlack _ = False
  in ((Tableau us (mempty, fst $ foldr go ([],fromIntegral $ Set.size allVars) allVars) u, f), undefined)

untransposeTab :: ((Tableau, Equality), Map.Map Integer String) -> (Tableau, Equality)
untransposeTab = undefined
