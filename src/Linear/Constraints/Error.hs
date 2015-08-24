{-# LANGUAGE
    FlexibleContexts
  #-}

module Linear.Constraints.Error where

import Linear.Class
import Linear.Constraints.Tableau
import Linear.Constraints.Cassowary
import Linear.Grammar
import Data.Set.Class as Sets

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Set as Set
import Data.Maybe
import Control.Applicative


-- * Error Variables

-- makeErrorVars :: (Tableau b, Equality b) -> (Tableau b, Equality b)
makeErrorVars (Tableau (BNFTableau bus, us) (BNFTableau sus, ss) u,f) =
  let toSub = Map.fromList $ fmap (\n ->
                ( VarMain n -- replace each Main var with its ErrVar equation
                , EquStd $ Equ (LinVarMap $ Map.fromList
                                  [ (VarError n ErrPos, one')
                                  , (VarError n ErrNeg, (-1 :: Rational) .*. one')
                                  ]) 0
                )) u
      newsus = Map.fromList $ mapMaybe (\u' ->
        (\equation -> ( VarError u' ErrPos -- basic in the positive err var
                      , mapVars (\(LinVarMap xs) -> LinVarMap $ -- basic in both error vars
                          xs `union` Map.singleton (VarError u' ErrNeg) one') equation -- because 0 = 1 + -1 ~ 1 = 1
               )) <$> Map.lookup u' bus) u -- "when this unrestricted var is basic..."
      bus' = bus `union` Map.mapKeys (\(VarMain n) -> n) toSub -- unrestricted
      us' = Map.foldWithKey (\k a -> fmap $ substitute k a) us toSub -- post-substitution
      ss' = Map.foldWithKey (\k a -> fmap $ substitute k a) ss toSub
      sus' = let basicRestricted = Map.foldWithKey (\k a -> fmap $ substitute k a) sus toSub
             in basicRestricted `union` newsus
      f' = unEquStd $ Map.foldWithKey substitute (EquStd f) toSub
  in (Tableau (BNFTableau bus', us') (BNFTableau sus', ss') u, f')
