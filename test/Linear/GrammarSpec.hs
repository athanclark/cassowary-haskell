module Linear.GrammarSpec (grammarSpec) where

import Linear.Grammar
import Linear.Grammar.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List hiding (union)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck


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
  ]

prop_multReduction_Idempotency :: LinAst Text Rational -> Bool
prop_multReduction_Idempotency x = multLin x == multLin (multLin x)

prop_addMutation_NonForgetful :: LinAst Text Rational -> Bool
prop_addMutation_NonForgetful x = Map.size (linExprVars $ addLin $ multLin x)
                               == length (nub $ astVars $ multLin x)
  where
    astVars :: LinAst Text Rational -> [Text]
    astVars (EVar n) = [n]
    astVars (ELit _) = []
    astVars (ECoeff _ 0) = []
    astVars (ECoeff e _) = astVars e
    astVars (EAdd e1 e2) = astVars e1 ++ astVars e2

prop_linVar_notNull :: LinVar Text Rational -> Bool
prop_linVar_notNull (LinVar (VarMain n) _) = not (T.null n)
prop_linVar_notNull (LinVar (VarRestricted (VarError n _)) _) = not (T.null n)
prop_linVar_notNull _ = True
