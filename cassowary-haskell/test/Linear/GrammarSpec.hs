module Linear.GrammarSpec (main, spec) where

import Linear.Grammar

import Test.Hspec
import Test.QuickCheck

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
  describe "LinExpr" $ do
    it "should generate unique variable names" $
      property prop_linExpr_uniquelyNamed
    it "`removeDupLin` should be idempotent" $
      property prop_removeDup_Idempotency
  describe "IneqStdForm" $
    it "should generate unique variable names" $
      property prop_ineqStdForm_uniquelyNamed
  describe "Ineq" $
    it "`standardize` should be idempotent" $
      property prop_standardize_Idempotency

prop_multReduction_Idempotency :: LinAst -> Bool
prop_multReduction_Idempotency x = multLin x == multLin (multLin x)

prop_addMutation_NonForgetful :: LinAst -> Bool
prop_addMutation_NonForgetful x = length (exprVars $ addLin $ multLin x)
                               == length (astVars $ multLin x)
  where
    astVars :: LinAst -> [String]
    astVars (EVar n) = [n]
    astVars (ELit _) = []
    astVars (ECoeff e _) = astVars e
    astVars (EAdd e1 e2) = astVars e1 ++ astVars e2

prop_linVar_notNull :: LinVar -> Bool
prop_linVar_notNull x = not $ null $ varName x

prop_linExpr_uniquelyNamed :: LinExpr -> Bool
prop_linExpr_uniquelyNamed x = hasNoDups $ map varName $ exprVars x

prop_removeDup_Idempotency :: LinExpr -> Bool
prop_removeDup_Idempotency x = removeDupLin x == removeDupLin (removeDupLin x)

prop_ineqStdForm_uniquelyNamed :: IneqStdForm -> Bool
prop_ineqStdForm_uniquelyNamed x = hasNoDups $ map varName $ getStdVars x

prop_standardize_Idempotency :: Ineq -> Bool
prop_standardize_Idempotency x = standardize x == standardize (standardize x)
