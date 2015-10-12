{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar

import qualified Data.IntMap as IntMap
import Data.STRef
import Data.Foldable
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.Base


-- * Slack Variables

-- | Turns all inequality expressions into equality expressions by populating
-- "slack" variables to compensate for the leeway removed:
--
-- >     x  >= c
-- > x + s1  = c
makeSlackVars :: ( Foldable f
                 , Num a
                 ) => f (IneqStdForm k a)
                   -> IntMap.IntMap (IneqStdForm k a)
makeSlackVars xs' = runST $ do
  k <- newSTRef 0
  runReaderT (foldlM mkSlackStdForm mempty xs') k
  where
    mkSlackStdForm :: ( MonadReader (STRef s Int) m
                      , MonadBase (ST s) m
                      , Num a
                      ) => IntMap.IntMap (IneqStdForm k a)
                        -> IneqStdForm k a
                        -> m (IntMap.IntMap (IneqStdForm k a))
    mkSlackStdForm acc (GteStd (Gte (LinExpr xs xc))) =
      mkSlackStdForm acc $ LteStd $ Lte $ LinExpr (fmap negate xs) $ negate xc
    mkSlackStdForm acc c = do
      k <- ask
      i <- liftBase $ readSTRef k
      liftBase $ modifySTRef k (+1)
      return $ case c of
        LteStd (Lte (LinExpr xs xc)) -> IntMap.insert i (EquStd $ Equ $ LinExpr xs xc) acc
        _                            -> IntMap.insert i c acc
