module Linear.Constraints.WeightSpec
  ( weightSpec
  ) where

import Linear.Constraints.Weights
import Data.Maybe (isNothing)

import qualified Data.Map as Map
import Data.Foldable (toList)

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Test.QuickCheck


weightSpec :: TestTree
weightSpec = testGroup "Linear.Constraints.Weight"
  [ testGroup "Addition"
    [ QC.testProperty "a + -a = 0" prop_self_add_negate_zero
    , QC.testProperty "[a + a + a .. a]_n ~ n `leftMult` a]" prop_self_add_n_mult
    ]
  , testGroup "Subtraction"
    [ QC.testProperty "a - a = 0" prop_self_sub_zero
    , QC.testProperty "a = -(-a)" prop_self_double_negation
    ]
  , testGroup "General"
    [ QC.testProperty "is Compact" prop_self_no_trailing_zero
    ]
  ]

prop_self_add_negate_zero :: Weight Rational -> Bool
prop_self_add_negate_zero xs = xs + (-xs) == 0

prop_self_add_n_mult :: Int -> Weight Rational -> Property
prop_self_add_n_mult n xs = n > 0 ==> sum (replicate n xs) == (fromIntegral n) `leftMultWeight` xs

prop_self_sub_zero :: Weight Rational -> Bool
prop_self_sub_zero xs = xs - xs == 0

prop_self_double_negation :: Weight Rational -> Bool
prop_self_double_negation xs = xs == negate (negate xs)

prop_self_no_trailing_zero :: Weight Rational -> Weight Rational -> Bool
prop_self_no_trailing_zero xs ys =
  noZero (toList $ xs + ys) && noZero (toList $ xs - ys)
  where
    noZero = snd . foldr noZero' ([],True)
    noZero' 0 ([],_) = ([0], False)
    noZero' x (xs,b) = (x:xs, b)

