module Linear.Cassowary.ClAbstractVariable where

import Linear.Cassowary.ClVariable (ClVariable (..))
import Linear.Cassowary.ClSlackVariable (ClSlackVariable (..))
import Linear.Cassowary.ClDummyVariable (ClDummyVariable (..))

import Data.Text (Text)

class ClAbstractVariable a where
  -- | Name of the variable, for debugging
  name :: a -> Text
  -- | Whether or not the variable is used for anything except a placeholder
  isDummy :: a -> Bool
  -- | Whether or not the variable is user-facing, and provided by the user
  isExternal :: a -> Bool
  -- | Whether or not we can pivot on this variable to become basic
  isPivotable :: a -> Bool
  -- | Whether or not the variable is restricted to be @>= 0@
  isRestricted :: a -> Bool

instance ClAbstractVariable ClVariable where
  name (ClVariable x) = x
  isDummy _ = False
  isExternal _ = True
  isPivotable _ = False
  isRestricted _ = False

instance ClAbstractVariable ClSlackVariable where
  name (ClSlackVariable x) = x
  isDummy _ = False
  isExternal _ = False
  isPivotable _ = True
  isRestricted _ = True

instance ClAbstractVariable ClDummyVariable where
  name (ClDummyVariable x) = x
  isDummy _ = True
  isExternal _ = False
  isPivotable _ = False
  isRestricted _ = True


