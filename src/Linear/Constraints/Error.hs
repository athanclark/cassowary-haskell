{-# LANGUAGE
    RankNTypes
  , ScopedTypeVariables
  #-}

module Linear.Constraints.Error where

-- import Linear.Class
import Linear.Constraints.Tableau (Tableau (Tableau))
import Linear.Constraints.Cassowary.AugmentedSimplex ()
import Linear.Grammar.Types
  ( IneqStdForm (EquStd)
  , Equality (Equ)
  , LinVarName (VarMain, VarRestricted)
  , RLinVarName (VarError)
  , LinExpr (LinExpr)
  )
-- import Data.Set.Class as Sets
-- import Data.Set.Unordered.Unique (unUUSet)
import qualified Data.Map as Map
-- import Data.Composition ((.*))


-- * Error Variables

-- | Replaces unrestricted variables with /two/ restricted variables - one for
-- the positive component and one for the negative component, whose difference
-- sums to be the original restricted variable:
--
-- > -∞ <= x <= ∞
-- >
-- > x_pos >= 0
-- > x_neg >= 0
-- >
-- > x = x_pos - x_neg
makeErrorVars :: forall k a c
               . ( Num a
                 ) => Tableau k k' a c
                   -> (Tableau k k' a c, Equality k a c)
makeErrorVars t@(Tableau (BNFTableau us, us') (BNFTableau rs, rs')) =
  let u = unUUSet (unrestrictedMainVars t)
      substitutions :: Map.Map (LinVarName k) (IneqStdForm (RLinVarName k) a c)
      substitutions =
        let errVarMapping :: LinVarName k -> (LinVarName k, IneqStdForm (RLinVarName k) a c)
            errVarMapping (VarMain n) =
              ( VarMain n -- from Main var to its ErrVar equation
              , EquStd $ Equ $ LinExpr (Map.fromList [(VarError n ErrPos,1), (VarError n ErrNeg,-1)]) 0
              )
        in  Map.fromList (errVarMapping <$> u)

      newErrs :: Map.Map (RLinVarName k) (IneqStdForm (RLinVarName k) a c)
      newErrs =
        -- Restricts unrestricted vars
        let errVarMapping :: LinVarName k -> Maybe (RLinVarName k, IneqStdForm )
            errVarMapping (VarMain n) = do
              equation <- Map.lookup n us
              pure
                ( VarError n ErrPos
                , ineqStdMapVars
                    (\xs -> xs `Map.union` Map.singleton (VarError n ErrNeg) 1)
                    equation
                )
        -- "for every basic unrestricted var, re-orient to the positive error var in basic form"
        in  Map.fromList (mapMaybe errVarMapping u)

      newus :: Map.Map k (IneqStdForm (RLinVarName k) a c)
      newus = us `union` Map.mapKeys (\(VarMain n) -> n) toSub -- unrestricted

      applySubstitutions :: forall f
                          . Functor f
                         => LinVarName k
                         -> IneqStdForm (RLinVarName k) a c
                         -> f (IneqStdForm (RLinVarName k) a c)
                         -> f (IneqStdForm (RLinVarName k) a c)
      applySubstitutions k equ set = fmap (substitute k equ) set

      newus'  = Map.foldrWithKey applySubstitutions us' toSub
      newrs'  = Map.foldrWithKey applySubstitutions rs' toSub

      newrs :: Map.Map (RLinVarName k) (IneqStdForm (RLinVarName k) a c)
      newrs = Map.foldrWithKey applySubstitutions rs toSub `Map.union` newErrs -- FIXME might assume disjoint?

      nameMapping :: Equality k a c
      nameMapping =
        let makeErrVars (VarMain n) = [(VarError n ErrPos, 1), (VarError n ErrNeg, 1)]
        in  Equ (LinExpr (Map.fromList (concatMap makeErrVars u)) 0)

  in  (Tableau (BNFTableau newus, newus') (BNFTableau newrs, newrs'), nameMapping)
