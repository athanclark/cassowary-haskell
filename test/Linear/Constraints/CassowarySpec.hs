{-# LANGUAGE
    TypeSynonymInstances
  , FlexibleInstances
  #-}

module Linear.Constraints.CassowarySpec where

import Linear.Constraints.Cassowary
import Linear.Grammar
import Linear.Class
import Linear.Constraints.Tableau
import Data.Set.Class as Sets

import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck


cassowarySpec :: TestTree
cassowarySpec = testGroup "Linear.Constraints.Cassowary"
  [ testGroup "`flatten`"
    [ QC.testProperty "is non-destructive" prop_flatten_nonDestroy
    , QC.testProperty "is idempotent" prop_flatten_idemp
    , QC.testProperty "results in 1" prop_flatten_1
    ]
  , testGroup "`substitute`"
    [ QC.testProperty "on self should result in all 0s" prop_substitute_self0
    , QC.testProperty "results in 0 for any target" prop_substitute_any0
    ]
  -- , unitTests
  ]

newtype IneqStdFormWithMember b = IneqStdFormWithMember
  {unIneqStdFormWithMember :: (LinVarName, IneqStdForm b)}
  deriving (Show, Eq)

instance (Num b, Eq b, Arbitrary b) => Arbitrary (IneqStdFormWithMember b) where
  arbitrary = do
    body <- arbitrary
    n <- oneof (map return $ Map.keys $ unLinVarMap $ vars body)
    return $ IneqStdFormWithMember (n,body)

prop_flatten_nonDestroy :: IneqStdFormWithMember Rational -> Bool
prop_flatten_nonDestroy (IneqStdFormWithMember (n,x)) =
  Map.size (unLinVarMap $ vars x) == Map.size (unLinVarMap $ vars $ flatten n x)

prop_flatten_idemp :: IneqStdFormWithMember Rational -> Bool
prop_flatten_idemp (IneqStdFormWithMember (n,x)) =
  flatten n x == flatten n (flatten n x)

prop_flatten_1 :: IneqStdFormWithMember Rational -> Bool
prop_flatten_1 (IneqStdFormWithMember (n,x)) =
  case Map.lookup n $ unLinVarMap $ vars $ flatten n x of
    Nothing -> False
    Just 1 -> True
    Just _ -> False

prop_substitute_self0 :: IneqStdFormWithMember Rational -> Bool
prop_substitute_self0 (IneqStdFormWithMember (n,x)) =
  null $ coeffVals $ substitute n (flatten n x) (flatten n x)

prop_substitute_any0 :: IneqStdFormWithMember Rational -> IneqStdForm Rational -> Bool
prop_substitute_any0 (IneqStdFormWithMember (n,x)) y =
  case Map.lookup n $ unLinVarMap $ vars $ substitute n (flatten n x) y of
    Nothing -> True
    Just _ -> False

-- unitTests :: TestTree
-- unitTests = testGroup "Unit Tests"
--   [ testCase "should pass Finite Mathematics Lesson 4, Example 1" $
--       let f1 = EVar "x" .+. EVar "y" .+. EVar "z" .<=. ELit 600
--           f2 = EVar "x" .+. (3 :: Rational) .*. EVar "y" .<=. ELit 600
--           f3 = (2 :: Rational) .*. EVar "x" .+. EVar "z" .<=. ELit 900
--           obj = EVar "M" .==. (60 :: Rational) .*. EVar "x" .+. (90 :: Rational) .*. EVar "y"
--                 .+. (300 :: Rational) .*. EVar "z"
--           t@(Tableau _ (c_s,_) _,_) = simplexPrimal
--             (makeRestrictedTableau [f1,f2,f3], unEquStd $ standardForm obj)
--           rs = remainingBasics t
--       in
--       rs `union` Map.mapKeys unLinVarName (basicFeasibleSolution c_s) @?=
--       Map.fromList [("M",180000),("z",600),("1",600),("2",300)]
--   , testCase "should pass Finite Mathematics Lesson 4, Example 2" $
--       let f1 = EVar "a" .+. EVar "b" .+. EVar "c" .<=. ELit 100
--           f2 = (5 :: Rational) .*. EVar "a" .+. (4 :: Rational) .*. EVar "b"
--                .+. (4 :: Rational) .*. EVar "c" .<=. ELit 480
--           f3 = (40 :: Rational) .*. EVar "a" .+. (20 :: Rational) .*. EVar "b"
--                .+. (30 :: Rational) .*. EVar "c" .<=. ELit 3200
--           obj = EVar "M" .==. (70 :: Rational) .*. EVar "a" .+. (210 :: Rational) .*. EVar "b"
--                 .+. (140 :: Rational) .*. EVar "c"
--           t@(Tableau _ (c_s,_) _,_) = simplexPrimal
--             (makeRestrictedTableau [f1,f2,f3], unEquStd $ standardForm obj)
--           rs = remainingBasics t
--       in
--       -- traceShow t $
--       rs `union` Map.mapKeys unLinVarName (basicFeasibleSolution c_s) @?=
--       Map.fromList [("M",21000),("b",100),("1",80),("2",1200)]
--   , testCase "should pass Example of Simplex Procedure" $
--       let f1 = (2 :: Rational) .*. EVar "x1" .+. EVar "x2" .+. EVar "x3" .<=. ELit 14
--           f2 = (4 :: Rational) .*. EVar "x1" .+. (2 :: Rational) .*. EVar "x2"
--                .+. (3 :: Rational) .*. EVar "x3" .<=. ELit 28
--           f3 = (2 :: Rational) .*. EVar "x1" .+. (5 :: Rational) .*. EVar "x2"
--                .+. (5 :: Rational) .*. EVar "x3" .<=. ELit 30
--           obj = EVar "Z" .==. EVar "x1" .+. (2 :: Rational) .*. EVar "x2"
--                 .+. (-1 :: Rational) .*. EVar "x3"
--           t@(Tableau _(c_s,_) _,_) = simplexPrimal
--             (makeRestrictedTableau [f1,f2,f3], unEquStd $ standardForm obj)
--           rs = remainingBasics t
--       in
--       rs `union` Map.mapKeys unLinVarName (basicFeasibleSolution c_s) @?=
--       Map.fromList [("Z",13),("x1",5),("x2",4)]
--   ]

instance Arbitrary Rational where
  arbitrary = between1000Rational
