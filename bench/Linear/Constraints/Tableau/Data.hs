module Linear.Constraints.Tableau.Data where

import Linear.Constraints.Data
import Linear.Grammar
import Linear.Class
import Linear.Constraints.Tableau


tableau1 = makeUnrestrictedTableau $ replicate 10 ineqStd1
tableau2 = makeUnrestrictedTableau $ replicate 10 ineqStd2
tableau3 = makeUnrestrictedTableau $ replicate 10 ineqStd3
tableau4 = makeUnrestrictedTableau $ replicate 10 ineqStd4
tableau5 = makeUnrestrictedTableau $ replicate 10 ineqStd5

obj = unEquStd $ standardForm $
      (10 :: Rational) .*. EVar "a"
  .+. (20 :: Rational) .*. EVar "b"
  .+. (30 :: Rational) .*. EVar "c"
  .==. ELit 40
