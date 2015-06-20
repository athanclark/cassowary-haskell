{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , DeriveFunctor
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar
import Linear.Grammar.Class
import Sets.Class

import qualified Data.Map as Map
import Control.Monad.State


makeSlackVars :: MonadState Integer m
              => [IneqStdForm]
              -> m [IneqStdForm]
makeSlackVars cs = do
  s <- get
  put $ s+1
  mapM mkSlackStdForm cs
  where
    mkSlackStdForm c = do
      s <- get
      put $ s+1
      return $ mapVars (\(LinVarMap xs) -> LinVarMap $
        xs `union` Map.singleton (VarSlack $ show s) 1) c
