module Linear.Cassowary.ClVariable where

import Data.Text (Text)

-- | User-facing variable
data ClVariable = ClVariable {clVariableName :: Text}
  -- FIXME how to get current value? Needs to be done with a ClSimplexSolver context?
