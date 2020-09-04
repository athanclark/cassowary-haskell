module Linear.Cassowary.ClDummyVariable where

import Data.Text (Text)

-- | Marker to allow equality constraints to be deleted. These are never
-- pivoted into the bias.
data ClDummyVariable = ClDummyVariable {clDummyVariableName :: Text}
