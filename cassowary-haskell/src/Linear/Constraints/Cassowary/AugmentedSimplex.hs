{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Linear.Constraints.Slack
import Linear.Constraints.Class
import Linear.Constraints.Tableau
import Linear.Grammar
import Linear.Grammar.Class

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State


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


f1 = EVar "x" .+. EVar "y" .+. EVar "z" .<=. ELit 600
f2 = EVar "x" .+. (3 :: Rational) .*. EVar "y" .<=. ELit 600
f3 = (2 :: Rational) .*. EVar "x" .+. EVar "z" .<=. ELit 900
obj = EVar "M" .==. (60 :: Rational) .*. EVar "x" .+. (90 :: Rational) .*. EVar "y"
      .+. (300 :: Rational) .*. EVar "z"
t = (makeRestrictedTableau [f1,f2,f3], unEquStd $ standardForm obj)
(Tableau _ (c_s,_) _,obj') = simplexPrimal t
s = fromJust $ Map.lookup (VarMain "M") $ unLinVarMap $ vars obj'


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
