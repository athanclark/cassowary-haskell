module Linear.Grammar.Types.Variables where

import Test.QuickCheck (Arbitrary (arbitrary), oneof, choose)


-- | Sign for error variables - converting a positive or negative number, to strictly a positive
-- number, flagged by its sign.
data ErrorSign = ErrNeg | ErrPos
  deriving (Show, Eq, Ord)

instance Arbitrary ErrorSign where
  arbitrary = boolToErrorSign <$> arbitrary
    where
      boolToErrorSign b = if b then ErrPos else ErrNeg

-- | /Restricted/ linear variable name types - only slack variables and error
-- variables are known to be @>= 0@.
data RLinVarName k =
    -- | Slack variable, with a unique name iterated by a stateful @Int@.
    VarSlack Int
  | -- | Error variable for stay and edit constraints, with its original name and the sign; either positive or negative.
    VarError k ErrorSign
  deriving (Show, Eq, Ord)

instance Arbitrary k => Arbitrary (RLinVarName k) where
  arbitrary = oneof
    [ VarSlack <$> choose (0, 1000)
    , VarError <$> arbitrary <*> arbitrary
    ]

-- | Type representing both restricted and unrestricted variables
data LinVarName k =
    VarMain k
  | VarRestricted (RLinVarName k)
  deriving (Eq, Ord)

-- | Not intended to be parsed afterward, just for debugging
instance Show k => Show (LinVarName k) where
  show x = case x of
    VarMain n -> show n
    VarRestricted (VarSlack n) -> show n
    VarRestricted (VarError n b) ->
      "error_"
        ++ show n
        ++ if b == ErrPos then "_+" else "_-"

instance Arbitrary k => Arbitrary (LinVarName k) where
  arbitrary = oneof
    [ VarMain <$> arbitrary
    , VarRestricted <$> arbitrary
    ]

-- | Linear variable
data LinVar k a = LinVar
  { varName  :: LinVarName k
  , varCoeff :: a
  } deriving (Show, Eq)

instance (Arbitrary a, Arbitrary k) => Arbitrary (LinVar k a) where
  arbitrary = LinVar <$> arbitrary <*> arbitrary

-- | Name-based ordering
instance (Eq a, Ord k) => Ord (LinVar k a) where
  compare (LinVar x _) (LinVar y _) = compare x y
