{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , TypeSynonymInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Linear.Grammar
import Linear.Grammar.Class

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State


type Constraint = () -- FIXME

data IneqSlack = IneqSlack
  { slackIneq :: IneqStdForm
  , slackVars :: LinVarMap
  } deriving (Show, Eq)

instance HasVariables IneqSlack LinVarMap where
  names (IneqSlack x xs) = names x ++ Map.keys xs
  mapNames f (IneqSlack x xs) = IneqSlack (mapNames f x) $ Map.mapKeys f xs
  vars (IneqSlack x xs) = vars x `Map.union` xs
  mapVars f (IneqSlack x xs) = IneqSlack (mapVars f x) $ f xs

instance HasCoefficients IneqSlack where
  coeffVals (IneqSlack x xs) = coeffVals x ++ coeffVals xs
  mapCoeffs f (IneqSlack x xs) = IneqSlack (mapCoeffs f x) (mapCoeffs f xs)

instance HasConstant IneqSlack where
  constVal (IneqSlack x _) = constVal x
  mapConst f (IneqSlack x xs) = IneqSlack (mapConst f x) xs

data EqualitySlack = EqualitySlack
  { slackEqu :: Equality
  , slackEquVars :: LinVarMap
  } deriving (Show, Eq)

makeSlackVars :: MonadState Integer m
              => [IneqStdForm]
              -> m [IneqSlack]
makeSlackVars cs = do
  s <- get
  put $ s+1
  mapM mkSlackStdForm cs
  where
    mkSlackStdForm c = do
      s <- get
      put $ s+1
      return $ IneqSlack c $ Map.singleton (show s) 1

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

type Unrestricted = [Constraint]

unrestricted :: [Constraint] -> Unrestricted
unrestricted cs = undefined -- filter

type Restricted = [Constraint]

restricted :: [Constraint] -> Restricted
restricted cs = undefined -- filter

-- | @x >= 0@
type Positives = [Constraint]

positives :: [Constraint] -> Positives
positives cs = undefined -- filter
