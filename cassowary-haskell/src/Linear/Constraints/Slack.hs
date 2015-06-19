{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar
import Linear.Grammar.Class

import qualified Data.Map as Map
import Control.Monad.State


-- * Slack variables

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
