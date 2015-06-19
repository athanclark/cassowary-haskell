module Linear.Constraints.Tableau where

import Linear.Constraints.Slack
import Linear.Grammar
import Linear.Grammar.Class



-- * Basic Normal Form

data BasicNormalForm = BasicNormalForm
 { basicSlack :: IneqSlack
 , basicVar :: LinVar -- ^ implicitly added to the map of variables in the standard-form equation
 }
