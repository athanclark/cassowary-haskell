{-# LANGUAGE
    FlexibleContexts
  , RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Constraints.Slack where

import Linear.Grammar.Types.Expressions (LinExpr (LinExpr))
import Linear.Grammar.Types.Inequalities
  ( IneqStdForm (EquStd, LteStd)
  , Equality (Equ)
  , LInequality (Lte)
  )

import qualified Data.IntMap as IntMap
import Data.STRef (STRef, modifySTRef, readSTRef, newSTRef)
import Data.Foldable (foldlM)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.Base (liftBase)


-- * Slack Variables

-- | Turns all inequality expressions into equality expressions by populating
-- "slack" variables to compensate for the leeway removed:
--
-- > x      >= c
-- > x + s1  = c
makeSlackVars :: forall f k a c
               . Foldable f
              => Num a
              => Num c
              => f (IneqStdForm k a c) -> IntMap.IntMap (IneqStdForm k a c)
makeSlackVars xs' = runST $ do
  k <- newSTRef 0
  runReaderT (foldlM mkSlackStdForm mempty xs') k
  where
    mkSlackStdForm :: IntMap.IntMap (IneqStdForm k a c)
                   -> IneqStdForm k a c
                   -> ReaderT (STRef s Int) (ST s) (IntMap.IntMap (IneqStdForm k a c))
    mkSlackStdForm acc c = do
      i <- do
        k <- ask
        liftBase $ do
          i' <- readSTRef k
          modifySTRef k (+1)
          pure i'
      pure $ case c of
        LteStd (Lte (LinExpr xs xc)) ->
          IntMap.insert i (EquStd $ Equ $ LinExpr xs xc) acc
        _ ->
          IntMap.insert i c acc
