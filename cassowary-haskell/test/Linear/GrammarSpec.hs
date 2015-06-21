module Linear.GrammarSpec (main, spec) where

import Linear.Grammar

import qualified Data.Map as Map

import Data.List hiding (union)
import Test.Hspec
import Test.QuickCheck

import Debug.Trace


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "LinAst" $ do
    it "`multLin` should be idempotent" $
      property prop_multReduction_Idempotency
    it "`addLin` should not add or remove variables" $
      property prop_addMutation_NonForgetful
  describe "LinVar" $
    it "should generate non-empty variable names" $
      property prop_linVar_notNull
  describe "Ineq" $
    it "`standardize` should be idempotent" $
      property prop_standardize_Idempotency

prop_multReduction_Idempotency :: LinAst -> Bool
prop_multReduction_Idempotency x = traceShow x $ multLin x == multLin (multLin x)

prop_addMutation_NonForgetful :: LinAst -> Bool
prop_addMutation_NonForgetful x = traceShow x $ Map.size (unLinVarMap $ exprVars $ addLin $ multLin x)
                               == length (nub $ astVars $ multLin x)
  where
    astVars :: LinAst -> [String]
    astVars (EVar n) = [n]
    astVars (ELit _) = []
    astVars (ECoeff e _) = astVars e
    astVars (EAdd e1 e2) = astVars e1 ++ astVars e2

prop_linVar_notNull :: LinVar -> Bool
prop_linVar_notNull (LinVar (VarMain n) _) = not $ null n
prop_linVar_notNull (LinVar (VarError n _) _) = not $ null n
prop_linVar_notNull _ = True

prop_standardize_Idempotency :: IneqExpr -> Bool
prop_standardize_Idempotency x = traceShow x $ standardize x == standardize (standardize x)
