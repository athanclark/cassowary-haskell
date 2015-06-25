{-# LANGUAGE
    StandaloneDeriving
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  #-}

module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.List (nub)
import Data.Maybe
import Data.Monoid
import Control.Monad.State
import Control.Applicative


newtype BNFTableau a = BNFTableau
  { unBNFTablaeu :: Map.Map a IneqStdForm
  } deriving (Show, Eq)

deriving instance (Ord a) => Monoid (BNFTableau a)

data Tableau = Tableau
  { unrestricted :: (BNFTableau String, IMap.IntMap IneqStdForm) -- ^ Unrestricted constraints include at least one of @urVars@.
  , restricted   :: (BNFTableau LinVarName, IMap.IntMap IneqStdForm)
  , urVars       :: [String]
  } deriving (Show, Eq)

basicFeasibleSolution :: BNFTableau a -> Map.Map a Rational
basicFeasibleSolution (BNFTableau solutions) =
  fmap constVal solutions

-- | Assumes all @VarMain@ to be @>= 0@
makeRestrictedTableau :: [IneqExpr] -> Tableau
makeRestrictedTableau xs =
  Tableau (BNFTableau Map.empty, mempty)
          (BNFTableau Map.empty, evalState (makeSlackVars $ IMap.fromList $
            [0..] `zip` map standardForm xs) 0)
          []

makeUnrestrictedTableau :: [IneqExpr] -> Tableau
makeUnrestrictedTableau xs =
  Tableau (BNFTableau Map.empty, evalState (makeSlackVars $ IMap.fromList $
            [0..] `zip` map standardForm xs) 0)
          (BNFTableau Map.empty, mempty)
          (concatMap names xs)

remainingBasics :: (Tableau, Equality) -> Map.Map String Rational
remainingBasics (Tableau (BNFTableau bus,us) (BNFTableau sus,ss) _, f) =
  let mknew x = Map.toList $ Map.mapKeys unLinVarName $
        fmap (const $ Just $ constVal x) $ unLinVarMap $ vars x
      allVars = foldr go mempty $ concatMap mknew $
                  EquStd f : IMap.elems us ++ Map.elems bus ++ IMap.elems ss ++ Map.elems sus
      allVars' = fromJust <$> Map.filter isJust allVars
  in Map.filter (/= 0) allVars'
  where
    go :: (String, Maybe Rational)
       -> Map.Map String (Maybe Rational)
       -> Map.Map String (Maybe Rational)
    go (k,v) acc = case Map.lookup k acc of
      Just (Just _) -> Map.update (const $ Just Nothing) k acc
      Just Nothing -> acc
      Nothing -> Map.insert k v acc
