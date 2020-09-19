module Linear.Constraints.Cassowary.Bland where

import Linear.Grammar.Types.Class (getVars, getConst)
import Linear.Grammar.Types.Inequalities (Equality (Equ), IneqStdForm)

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (guard)


-- | Bland's method.
--
-- Notice that the coefficient and constant types are the same, because we
-- divide the constant by the coefficient.
blandRatioPrimal :: ( Ord k
                    , Fractional a
                    , Ord a
                    ) => k
                      -> IneqStdForm k a a -- ^ Row
                      -> Maybe a
blandRatioPrimal var row = do
  coeff <- Map.lookup var (getVars row)
  guard (coeff < 0)
  pure (negate (getConst row) / coeff)

-- | Bland's method, for maximal optimization.
blandRatioDual :: ( Ord k
                  , Ord a
                  , Num a
                  , Fractional a
                  ) => k
                    -> Equality k a c
                    -> IneqStdForm k a c
                    -> Maybe a
blandRatioDual var (Equ objective) row = do
  let o = fromMaybe 0 (Map.lookup var (getVars objective))
  x <- Map.lookup var (getVars row)
  guard (x > 0)
  pure (o / x)
