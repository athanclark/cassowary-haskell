module Linear.Constraints.Class where

import Linear.Grammar
import Linear.Grammar.Class


class HasMainVars a where
  mainVars :: a -> LinVarMap
  mapMainVars :: (LinVarMap -> LinVarMap) -> a -> a

instance HasMainVars IneqStdForm where
  mainVars = vars
  mapMainVars = mapVars

class HasIneq a where
  ineqStd :: a -> IneqStdForm

instance HasIneq IneqStdForm where
  ineqStd = id

class HasSlackVars a where
  slackVars :: a -> LinVarMap
  mapSlackVars :: (LinVarMap -> LinVarMap) -> a -> a

class HasErrorVars a where
  errorVars :: a -> LinVarMap
  mapErrorVars :: (LinVarMap -> LinVarMap) -> a -> a
