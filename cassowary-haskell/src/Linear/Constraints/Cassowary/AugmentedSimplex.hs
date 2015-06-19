{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Grammar.Class

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State


-- * Bland's Rule

-- | Most negative coefficient in objective function
nextBasic :: Equality -> Maybe String
nextBasic (Equ xs _) =
  let x = minimumBy (\(_,v) (_,v') -> compare v v') $ Map.toList xs
  in if snd x < 0
     then Just $ fst x
     else Nothing

-- | Finds the index of the next row to pivot on - note, list must have
nextRow :: String -> [IneqSlack] -> Maybe Int
nextRow _ [] = Nothing
nextRow col xs = elemIndex smallest $ map (blandRatio col) xs
  where
    smallest = minimum <$> traverse (blandRatio col) xs

-- | Using Bland's method.
blandRatio :: String -> IneqSlack -> Maybe Rational
blandRatio col x = Map.lookup col (vars x) >>=
  \coeff -> Just $ constVal x / coeff

-- | Orients equation over some (existing) variable
flatten :: String -> IneqSlack -> IneqSlack
flatten col (IneqSlack x slacks) = case Map.lookup col $ vars x of
  Just y -> IneqSlack (mapCoeffs (map (/ y)) x) $ Map.map (/ y) slacks
  Nothing -> error "`flatten` should be called with a variable that exists in the equation"

-- | Takes a flattened focal row and a target row, and removes the focal from the target.
eliminate :: String -> IneqSlack -> IneqSlack -> IneqSlack
eliminate col focal target = case Map.lookup col $ vars $ slackIneq target of
  Just coeff -> let focal' = mapCoeffs (map (* coeff)) focal
                    go xs = let xs' = Map.unionWith (-) xs (vars focal')
                            in Map.filter (/= 0) xs'
    in mapVars go target
  Nothing -> target



-- | Performs a single pivot
pivot :: ([IneqSlack], Equality) -> Maybe ([IneqSlack], Equality)
pivot (cs,f) = let mCol = nextBasic f
                   mRow = mCol >>= (`nextRow` cs)
  in case (mCol, mRow) of
       (Just col, Just row) -> let csPre = take row cs
                                   csPost = drop (row+1) cs
                                   focal = flatten col $ cs !! row
          in Just ( map (eliminate col focal) csPre
                 ++ [focal]
                 ++ map (eliminate col focal) csPost
                  , case eliminate col focal $ IneqSlack (EquStd f) Map.empty of
                      (IneqSlack (EquStd x) _) -> x
                  )
       _ -> Nothing

-- | Simplex optimization
optimize :: ([IneqSlack], Equality) -> ([IneqSlack], Equality)
optimize x = case pivot x of
  Just (cs,f) -> optimize (cs,f)
  Nothing -> x

-- type Unrestricted = [Constraint]
--
-- unrestricted :: [Constraint] -> Unrestricted
-- unrestricted cs = undefined -- filter
--
-- type Restricted = [Constraint]
--
-- restricted :: [Constraint] -> Restricted
-- restricted cs = undefined -- filter
--
-- -- | @x >= 0@
-- type Positives = [Constraint]
--
-- positives :: [Constraint] -> Positives
-- positives cs = undefined -- filter
