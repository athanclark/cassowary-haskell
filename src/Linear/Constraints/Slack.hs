{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar
import Data.Set.Class as Sets

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.STRef
import Data.Traversable (traverse)
import Control.Monad.ST


makeSlackVars :: ( Num b
                 ) => IntMap.IntMap (IneqStdForm b) -> IntMap.IntMap (IneqStdForm b)
makeSlackVars xs' = runST $ do
  i <- newSTRef 0
  traverse (mkSlackStdForm i) xs'
  where
    mkSlackStdForm :: ( Num b
                      ) => STRef s Integer -> IneqStdForm b -> ST s (IneqStdForm b)
    mkSlackStdForm _ (EquStd c) = return $ EquStd c
    mkSlackStdForm i (LteStd (Lte (LinVarMap xs) xc)) = do
      s <- readSTRef i
      writeSTRef i $ s+1
      return $ EquStd $ Equ (LinVarMap $ xs `union` Map.singleton (VarSlack s) 1) xc
    mkSlackStdForm i (GteStd (Gte (LinVarMap xs) xc)) =
      mkSlackStdForm i $ LteStd $ Lte (LinVarMap $ fmap (* (-1)) xs) $ xc * (-1)
