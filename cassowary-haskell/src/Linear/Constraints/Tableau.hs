module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Grammar.Class

import qualified Data.Map as Map


-- * Shorthand References

-- | Unbounded variables in constraints
type Unrestricted = [BasicNormalForm]

unrestricted :: [BasicNormalForm] -> Unrestricted
unrestricted cs = undefined -- filter

-- | Involves constraints where all vars are positive
type Restricted = [BasicNormalForm]

restricted :: [BasicNormalForm] -> Restricted
restricted cs = undefined -- filter

-- | @x >= 0@
type Positives = [BasicNormalForm]

positives :: [BasicNormalForm] -> Positives
positives cs = undefined -- filter

-- * Error Variables

data LinErrorVar = LinErrorVar
  { errorCoeff :: Rational
  , errorPlus  :: String
  , errorMinus :: String
  } deriving (Show, Eq)

-- * Basic Normal Form

data BasicLinVar =
    BasicNorm  LinVar
  | BasicSlack LinVar
  | BasicError LinVar
  deriving (Show, Eq, Ord)

newtype BNFTableau = BNFTableau
 { unBNFTablaeu :: Map.Map BasicLinVar
 } deriving (Show, Eq)
