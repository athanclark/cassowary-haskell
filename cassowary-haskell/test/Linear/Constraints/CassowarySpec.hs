module Linear.Constraints.CassowarySpec where

import Linear.Constraints.Cassowary
import Linear.Grammar
import Linear.Constraints.Slack
import Linear.Constraints.Tableau
import Linear.Constraints.Class

import qualified Data.Set as Set
import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck


cassowarySpec :: TestTree
cassowarySpec = testGroup "Linear.Constraints.Cassowary"
  [ QC.testProperty "`flatten` is non-destructive"
      prop_flatten_nonDestroy
  ]

newtype IneqStdFormWithMember = IneqStdFormWithMember
  {unIneqStdFormWithMember :: (LinVarName, IneqStdForm)}
  deriving (Show, Eq)

instance Arbitrary IneqStdFormWithMember where
  arbitrary = do
    body <- arbitrary
    n <- oneof (map return $ Map.keys $ unLinVarMap $ mainVars body)
    return $ IneqStdFormWithMember (n,body)

prop_flatten_nonDestroy :: IneqStdFormWithMember -> Bool
prop_flatten_nonDestroy (IneqStdFormWithMember (n,x)) =
  Map.size (unLinVarMap $ mainVars x) == Map.size (unLinVarMap $ mainVars $ flatten n x)
