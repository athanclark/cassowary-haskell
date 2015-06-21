module Main where

import Linear.GrammarSpec

import Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Testing..." [grammarSpec]
