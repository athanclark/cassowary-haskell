{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Linear.Grammar

import Data.List
import qualified Data.Map as Map
import Control.Monad.State


type Constraint = () -- FIXME

data IneqSlack = IneqSlack
  { slackIneq :: IneqStdForm
  , slackVars :: LinVarMap
  } deriving (Show, Eq)

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

nextRow :: String -> [IneqSlack] -> Maybe Int

blandRatio :: String -> IneqStdForm -> Maybe Rational
blandRatio c x = 

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
