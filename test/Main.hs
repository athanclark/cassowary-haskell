module Main where

import Linear.GrammarSpec
import Linear.Constraints.SlackSpec
import Linear.Constraints.WeightSpec
import Linear.Constraints.CassowarySpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..."
  [ grammarSpec
  , slackSpec
  , weightSpec
  , cassowarySpec
  ]
