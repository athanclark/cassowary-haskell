module Linear.Constraints.Data where

import Linear.Grammar
import Linear.Class


ineqStdChunk1 = (5 :: Rational) .*. EVar "a"
            .+. (10 :: Rational) .*. EVar "b"
            .+. (15 :: Rational) .*. EVar "c"
            .+. ELit 15

ineqStdChunk2 = (5 :: Rational) .*. EVar "a"
            .+. (10 :: Rational) .*. EVar "b"
            .+. (15 :: Rational) .*. EVar "c"
            .+. ELit 20

ineqStd1 = standardForm $
              ineqStdChunk1
         .<=. ineqStdChunk2

ineqStd2 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
         .<=. ineqStdChunk2
          .+. ineqStdChunk2

ineqStd3 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
         .<=. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2

ineqStd4 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
         .<=. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2

ineqStd5 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .<=. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2


equStd1 = standardForm $
              ineqStdChunk1
         .==. ineqStdChunk2

equStd2 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
         .==. ineqStdChunk2
          .+. ineqStdChunk2

equStd3 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
         .==. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2

equStd4 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
         .==. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2

equStd5 = standardForm $
              ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
          .+. ineqStdChunk1
         .==. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
          .+. ineqStdChunk2
