module Linear.Grammar.Types.Syntax where

import Data.String (IsString (fromString))
import Test.QuickCheck (Arbitrary (arbitrary), Gen, sized, resize, scale, oneof, choose)


-- | User-facing abstract syntax tree, polymorphic in the numeric type used.
data LinAst k a =
    -- | Variable names
    EVar k
  | -- | Numeric literals
    ELit a
  | -- | A literal coefficient multiplied by some abstract syntax tree
    ECoeff (LinAst k a) a
  | -- | Two abstract syntax trees added together
    EAdd (LinAst k a) (LinAst k a)
  deriving (Show, Eq)

instance (Arbitrary a, Arbitrary k) => Arbitrary (LinAst k a) where
  arbitrary = sized go
    where
      go :: Arbitrary a => Arbitrary k => Int -> Gen (LinAst k a)
      go s
        | s <= 1 = oneof
          [ EVar <$> arbitrary
          , ELit <$> arbitrary
          ]
        | otherwise = oneof
          [ EVar <$> arbitrary
          , ELit <$> arbitrary
          , ECoeff <$> scale (subtract 1) arbitrary <*> arbitrary
          , do
              n <- choose (0,s-1)
              EAdd <$> resize n arbitrary <*> resize n arbitrary
          ]

instance IsString k => IsString (LinAst k a) where
  fromString = EVar . fromString


-- | Pushes 'ECoeff' down the tree, leaving 'EAdd' at the top level.
-- After using this funciton, all 'ECoeff' constructors\' 'LinAst' parameter will
-- be an 'EVar'.
multLin :: Num a => LinAst k a -> LinAst k a
multLin (EVar n) = EVar n
multLin (ELit x) = ELit x
multLin (ECoeff e x) = case multLin e of
  (ELit y)      -> ELit (y * x) -- evaluate coefficient multiplied by literal
  (EVar n)      -> ECoeff (EVar n) x -- do nothing
  (ECoeff e' y) -> ECoeff e' (y * x) -- redundant coefficients
  (EAdd e1 e2)  -> EAdd (multLin (ECoeff e1 x)) (multLin (ECoeff e2 x)) -- recurse
multLin (EAdd e1 e2) = EAdd (multLin e1) (multLin e2)
