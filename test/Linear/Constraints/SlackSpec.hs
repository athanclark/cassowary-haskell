{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  #-}

module Linear.Constraints.SlackSpec where

import Prelude hiding (all)

import Linear.Grammar
import Linear.Grammar.Types
import Linear.Constraints.Slack

import qualified Data.IntMap as IMap
import Data.Foldable (all)
import Data.Text (Text)
import Control.Monad.State

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Instances ()


slackSpec :: TestTree
slackSpec = testGroup "Linear.Constraints.Slack"
  [ QC.testProperty "`makeSlackVars` should result in only Equalities"
      prop_makeSlackVars_equ
  ]


prop_makeSlackVars_equ :: [IneqStdForm (LinVarName Text) Rational Rational] -> Bool
prop_makeSlackVars_equ x = all isEqu (makeSlackVars x)
  where
    isEqu :: IneqStdForm k b c -> Bool
    isEqu (EquStd _) = True
    isEqu _ = False
