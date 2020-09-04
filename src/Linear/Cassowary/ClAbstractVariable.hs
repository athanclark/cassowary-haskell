module Linear.Cassowary.ClAbstractVariable where

import Linaer.Cassowary.ClVariable (ClVariable (..))
import Linear.Cassowary.ClSlackVariable (ClSlackVariable (..))
import Linear.Cassowary.ClDummyVariable (ClDummyVariable (..))

import Data.Text (Text)

class ClAbstractVariable a where
  -- | Name of the variable, for debugging
  name :: a -> Text
  -- | Whether or not the variable is used for anything except a placeholder
  isDummy :: a -> Boolean
  -- | Whether or not the variable is user-facing, and provided by the user
  isExternal :: a -> Boolean
  -- | Whether or not we can pivot on this variable to become basic
  isPivotable :: a -> Boolean
  -- | Whether or not the variable is restricted to be @>= 0@
  isRestricted :: a -> Boolean

instance ClAbstractVariable ClVariable where
  name (ClVariable x) = x
  isDummy _ = false
  isExternal _ = true
  isPivotable _ = false
  isRestricted _ = false

instance ClAbstractVariable ClSlackVariable where
  name (ClSlackVariable x) = x
  isDummy _ = false
  isExternal _ = false
  isPivotable _ = true
  isRestricted _ = true

instance ClAbstractVariable ClDummyVariable where
  name (ClDummyVariable x) = x
  isDummy _ = true
  isExternal _ = false
  isPivotable _ = false
  isRestricted _ = true


