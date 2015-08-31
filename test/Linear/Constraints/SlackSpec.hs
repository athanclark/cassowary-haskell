{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  #-}

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


prop_makeSlackVars_equ :: [IneqStdForm Rational] -> Bool
prop_makeSlackVars_equ x = all isEqu $ makeSlackVars x
  where
    isEqu :: IneqStdForm b -> Bool
    isEqu (EquStd _) = True
    isEqu _ = False

prop_makeSlackVars_idemp :: [IneqStdForm Rational] -> Bool
prop_makeSlackVars_idemp x =
  let x1 = makeSlackVars x
      x2 = makeSlackVars $ makeSlackVars x
  in x1 == x2



instance Arbitrary Rational where
  arbitrary = between1000Rational
