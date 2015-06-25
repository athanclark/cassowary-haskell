{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar
import Sets.Class

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.STRef
import Data.Traversable (traverse)
import Control.Monad.ST
import Control.Applicative


-- makeSlackVars :: ( MonadState Integer m
--                  , Applicative m
--                  , Num b
--                  ) => IMap.IntMap (IneqStdForm b)
--               -> m (IMap.IntMap (IneqStdForm b))
makeSlackVars xs = runST $ do
  n <- newSTRef 0
  traverse (mkSlackStdForm n) xs
  where
    -- mkSlackStdForm :: ( MonadState Integer m
    --                   , Applicative m
    --                   , Num b
    --                   ) => IneqStdForm b -> m (IneqStdForm b)
    mkSlackStdForm n (EquStd c) = return $ EquStd c
    mkSlackStdForm n (LteStd (Lte (LinVarMap xs) xc)) = do
      s <- readSTRef n
      writeSTRef n $ s+1
      return $ EquStd $ Equ (LinVarMap $ xs `union` Map.singleton (VarSlack s) 1) xc
    mkSlackStdForm n (GteStd (Gte (LinVarMap xs) xc)) =
      mkSlackStdForm n $ LteStd $ Lte (LinVarMap $ fmap (* (-1)) xs) $ xc * (-1)
