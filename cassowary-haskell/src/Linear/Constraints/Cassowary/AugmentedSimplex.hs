{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Cassowary.AugmentedSimplex where

import Linear.Grammar

import Control.Monad.State


type Constraint = () -- FIXME

data IneqSlack = IneqSlack
  { slackIneq :: IneqStdForm
  , slackVars :: [LinVar]
  } deriving (Show, Eq)

data EqualitySlack = EqualitySlack
  { slackEqu :: Equality
  , slackEquVars :: [LinVar]
  } deriving (Show, Eq)

data FreshnessState = FreshnessState
  { freshSuffix :: Integer  -- new number each slack var as a suffix
  , varsInScope :: [String] -- ^ unique list of vars used already in constraint set
  } deriving (Show, Eq)

makeSlackVars :: MonadState FreshnessState m
              => ([IneqStdForm], Equality)
              -> m ([IneqSlack], EqualitySlack)
makeSlackVars (cs, f) = undefined -- adopt from simplex-basic

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
