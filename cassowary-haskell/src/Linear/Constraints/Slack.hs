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
makeSlackVars = mapM mkSlackStdForm
  where
    mkSlackStdForm :: MonadState Integer m => IneqStdForm -> m IneqStdForm
    mkSlackStdForm (EquStd c) = return $ EquStd c
    mkSlackStdForm (LteStd (Lte (LinVarMap xs) xc)) = do
      s <- get
      put $ s+1
      return $ EquStd $ Equ (LinVarMap $ xs `union` Map.singleton (VarSlack s) 1) xc
    mkSlackStdForm (GteStd (Gte (LinVarMap xs) xc)) =
      mkSlackStdForm $ LteStd $ Lte (LinVarMap $ fmap (* (-1)) xs) $ xc * (-1)
