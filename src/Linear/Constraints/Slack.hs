{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar
import Linear.Class

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
                 ) => f (IneqStdForm Rational)
                   -> IntMap.IntMap (IneqStdForm Rational)
makeSlackVars xs' = runST $ do
  k <- newSTRef 0
  runReaderT (foldlM mkSlackStdForm mempty xs') k
  where
    mkSlackStdForm :: ( MonadReader (STRef s Int) m
                      , MonadBase (ST s) m
                      ) => IntMap.IntMap (IneqStdForm Rational)
                        -> IneqStdForm Rational
                        -> m (IntMap.IntMap (IneqStdForm Rational))
    mkSlackStdForm acc (GteStd (Gte (LinVarMap xs) xc)) =
      mkSlackStdForm acc $ LteStd $ Lte (LinVarMap $ fmap negate xs) $ negate xc
    mkSlackStdForm acc c = do
      k <- ask
      i <- liftBase $ readSTRef k
      liftBase $ modifySTRef k (+1)
      return $ case c of
        LteStd (Lte xs xc) -> IntMap.insert i (EquStd $ Equ xs xc) acc
        _                  -> IntMap.insert i c acc
