module Linear.Constraints.Error where

import Linear.Constraints.Tableau
import Linear.Constraints.Class
import Linear.Constraints.Cassowary
import Linear.Grammar
import Linear.Grammar.Class
import Sets.Class

import qualified Data.Map as Map
import Data.Maybe


-- * Error Variables

makeErrorVars :: (Tableau, Equality) -> (Tableau, Equality)
makeErrorVars (Tableau (BNFTableau bus, us) (BNFTableau sus, ss) u,f) =
  let toSub = Map.fromList $ map (\n ->
                ( VarMain n
                , EquStd $ Equ (LinVarMap $ Map.fromList
                                  [ (VarError n True, 1)
                                  , (VarError n False, -1)
                                  ]) 0
                )) u
      newBasic = Map.fromList $ mapMaybe (\u' ->
        (\a -> ( VarError u' True
               , mapMainVars (\(LinVarMap xs) -> LinVarMap $ -- basic in both error vars
                    xs `union` Map.singleton (VarError u' False) 1) a
               )) <$> Map.lookup u' bus) u
      bus' = bus `union` Map.mapKeys (\(VarMain n) -> n) toSub
      us' = Map.foldWithKey (\k a acc -> map (substitute k a) acc) us toSub
      sus' = let basicRestricted = Map.foldWithKey (\k a acc -> fmap (substitute k a) acc) sus toSub
             in basicRestricted `union` newBasic
      ss' = Map.foldWithKey (\k a acc -> map (substitute k a) acc) ss toSub
      f' = unEquStd $ Map.foldWithKey substitute (EquStd f) toSub
  in (Tableau (BNFTableau bus', us') (BNFTableau sus', ss') u, f')
