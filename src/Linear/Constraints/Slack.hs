{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar
import Linear.Class
import Data.Set.Class as Sets

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.STRef
import Data.Traversable (traverse)
import Data.Foldable
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Base


-- | Turns all inequality expressions into equality expressions by populating
-- "slack" variables to compensate for the leeway removed.
makeSlackVars :: ( Foldable f
                 , HasNegate b
                 ) => f (IneqStdForm b) -> IntMap.IntMap (IneqStdForm b)
makeSlackVars xs' = runST $ do
  k <- newSTRef 0
  runReaderT (foldlM mkSlackStdForm mempty xs') k
  where
    mkSlackStdForm :: ( MonadReader (STRef s Int) m
                      , MonadBase (ST s) m
                      , HasNegate b
                      ) => IntMap.IntMap (IneqStdForm b) -> IneqStdForm b -> m (IntMap.IntMap (IneqStdForm b))
    mkSlackStdForm acc (GteStd (Gte (LinVarMap xs) xc)) =
      mkSlackStdForm acc $ LteStd $ Lte (LinVarMap $ fmap negate' xs) $ negate' xc
    mkSlackStdForm acc c = do
      k <- ask
      i <- liftBase $ readSTRef k
      liftBase $ modifySTRef k (+1)
      return $ case c of
        LteStd (Lte xs xc) -> IntMap.insert i (EquStd $ Equ xs xc) acc
        c                  -> IntMap.insert i c acc
