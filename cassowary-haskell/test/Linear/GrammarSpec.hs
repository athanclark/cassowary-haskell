module Linear.GrammarSpec (grammarSpec) where

import Linear.Grammar

import qualified Data.Map as Map

import Data.List hiding (union)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

import Debug.Trace


grammarSpec :: TestTree
grammarSpec = testGroup "Linear.Grammar"
  [ testGroup "LinAst"
      [ QC.testProperty "`multLin` should be idempotent"
          prop_multReduction_Idempotency
      , QC.testProperty "`addLin` should not add or remove variables"
          prop_addMutation_NonForgetful
      ]
  , testGroup "LinVar"
      [ QC.testProperty "should generate non-empty variable names"
          prop_linVar_notNull
      ]
  , testGroup "Ineq"
      [ QC.testProperty "`standardize` should be idempotent"
          prop_standardize_Idempotency
      ]
  ]

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
