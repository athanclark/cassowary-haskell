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
nextRow col xs = elemIndex smallest $ map (blandRatio col) xs
  where
    smallest = minimum <$> traverse (blandRatio col) xs

-- | Using Bland's method.
blandRatio :: ( HasConstant a
              , HasMainVars a
              ) => LinVarName -> a -> Maybe Rational
blandRatio col x = Map.lookup col (unLinVarMap $ mainVars x) >>=
  \coeff -> Just $ constVal x / coeff

-- | Orients equation over some (existing) variable
flatten :: ( HasCoefficients a
           , HasMainVars a
           ) => LinVarName -> a -> a
flatten col x = case Map.lookup col $ unLinVarMap $ mainVars x of
  Just y -> mapCoeffs (map (/ y)) x
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
                  in mapConst (* coeff) $ mapMainVars go target
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
                  , case substitute col focal $ EquStd f of
                      (EquStd x) -> x
                  )
       _ -> Nothing


-- | Simplex optimization
optimize :: (Tableau, Equality) -> (Tableau, Equality)
optimize x = case pivot x of
 Just (cs,f) -> optimize (cs,f)
 Nothing -> x
