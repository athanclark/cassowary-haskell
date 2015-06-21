module Linear.Constraints.SlackSpec where

import Linear.Grammar
import Linear.Constraints.Slack

import Control.Monad.State

import Test.Tasty
import Test.Tasty.QuickCheck as QC


slackSpec :: TestTree
slackSpec = testGroup "Linear.Constraints.Slack"
  [ QC.testProperty "`makeSlackVars` should result in only Equalities"
      prop_makeSlackVars_equ
  ]


prop_makeSlackVars_equ :: [IneqStdForm] -> Bool
prop_makeSlackVars_equ x = all isEqu $ evalState (makeSlackVars x) 0
  where
    isEqu :: IneqStdForm -> Bool
    isEqu (EquStd _) = True
    isEqu _ = False
