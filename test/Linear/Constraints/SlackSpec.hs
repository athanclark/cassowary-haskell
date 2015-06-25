module Linear.Constraints.SlackSpec where

import Prelude hiding (all)

import Linear.Grammar
import Linear.Constraints.Slack

import qualified Data.IntMap as IMap
import Data.Foldable (all)
import Control.Monad.State

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()


slackSpec :: TestTree
slackSpec = testGroup "Linear.Constraints.Slack"
  [ QC.testProperty "`makeSlackVars` should result in only Equalities"
      prop_makeSlackVars_equ
  , QC.testProperty "`makeSlackVars` should be idempotent"
      prop_makeSlackVars_idemp
  ]


prop_makeSlackVars_equ :: IMap.IntMap IneqStdForm -> Bool
prop_makeSlackVars_equ x = all isEqu $ evalState (makeSlackVars x) 0
  where
    isEqu :: IneqStdForm -> Bool
    isEqu (EquStd _) = True
    isEqu _ = False

prop_makeSlackVars_idemp :: IMap.IntMap IneqStdForm -> Bool
prop_makeSlackVars_idemp x =
  let x1 = evalState (makeSlackVars x) 0
      x2 = evalState (makeSlackVars =<< makeSlackVars x) 0
  in x1 == x2
