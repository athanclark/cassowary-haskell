module Linear.Grammar.Types.Data where

import Linear.Grammar
import Linear.Class



linAst1 = (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"

linAst2 = (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"

linAst3 = (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"

linAst4 = (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"

linAst5 = (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
      .+. (5 :: Rational) .*. EVar "a"
      .+. (10 :: Rational) .*. EVar "b"
      .+. (15 :: Rational) .*. EVar "c"
