module Linear.Cassowary.ClSlackVariable where

import Data.Text (Text)

-- | Variable that's restricted to be non-negative, either slack variables for
-- inequalities, or as error variables for non-required constraints.
data ClSlackVariable = ClSlackVariable {clSlackVariableName :: Text}
