module Main where

import Linear.Grammar.Types.Data
import Linear.Constraints.Data
import Linear.Constraints.Tableau.Data

import Linear.Grammar
import Linear.Class
import Linear.Constraints.Cassowary
import Linear.Constraints.Slack
import Linear.Constraints.Error

import Criterion.Main


main :: IO ()
main = defaultMain
  [ bgroup "Grammar" [ bgroup "multLin" [ bench "1" $ nf multLin linAst1
                                        , bench "2" $ nf multLin linAst2
                                        , bench "3" $ nf multLin linAst3
                                        , bench "4" $ nf multLin linAst4
                                        , bench "5" $ nf multLin linAst5
                                        ]
                     , bgroup "addLin" [ bench "1" $ whnf (addLin . multLin) linAst1
                                       , bench "2" $ whnf (addLin . multLin) linAst2
                                       , bench "3" $ whnf (addLin . multLin) linAst3
                                       , bench "4" $ whnf (addLin . multLin) linAst4
                                       , bench "5" $ whnf (addLin . multLin) linAst5
                                       ]
                     , bgroup "1 .==." [ bench "1" $ whnf (linAst1 .==.) linAst1
                                       , bench "2" $ whnf (linAst1 .==.) linAst2
                                       , bench "3" $ whnf (linAst1 .==.) linAst3
                                       , bench "4" $ whnf (linAst1 .==.) linAst4
                                       , bench "5" $ whnf (linAst1 .==.) linAst5
                                       ]
                     , bgroup ".==. 1" [ bench "1" $ whnf (.==. linAst1) linAst1
                                       , bench "2" $ whnf (.==. linAst1) linAst2
                                       , bench "3" $ whnf (.==. linAst1) linAst3
                                       , bench "4" $ whnf (.==. linAst1) linAst4
                                       , bench "5" $ whnf (.==. linAst1) linAst5
                                       ]
                     , bgroup "standardForm" [ bench "1" $ whnf (\x -> standardForm $ linAst1 .==. x) linAst1
                                             , bench "2" $ whnf (\x -> standardForm $ linAst1 .==. x) linAst2
                                             , bench "3" $ whnf (\x -> standardForm $ linAst1 .==. x) linAst3
                                             , bench "4" $ whnf (\x -> standardForm $ linAst1 .==. x) linAst4
                                             , bench "5" $ whnf (\x -> standardForm $ linAst1 .==. x) linAst5
                                             ]
                     ]
  , bgroup "Constraints" [ bgroup "makeSlackVars" [ bench "10 x 1" $ whnf (makeSlackVars . replicate 10) ineqStd1
                                                  , bench "10 x 2" $ whnf (makeSlackVars . replicate 10) ineqStd2
                                                  , bench "10 x 3" $ whnf (makeSlackVars . replicate 10) ineqStd3
                                                  , bench "10 x 4" $ whnf (makeSlackVars . replicate 10) ineqStd4
                                                  , bench "10 x 5" $ whnf (makeSlackVars . replicate 10) ineqStd5
                                                  ]
                         , bgroup "makeErrorVars" [ bench "10 x 1" $ whnf makeErrorVars (tableau1, obj)
                                                  , bench "10 x 2" $ whnf makeErrorVars (tableau2, obj)
                                                  , bench "10 x 3" $ whnf makeErrorVars (tableau3, obj)
                                                  , bench "10 x 4" $ whnf makeErrorVars (tableau4, obj)
                                                  , bench "10 x 5" $ whnf makeErrorVars (tableau5, obj)
                                                  ]
                         , bgroup "Simplex" [ bgroup "nextBasicPrimal" [ bench "1" $ whnf (nextBasicPrimal . unEquStd) equStd1
                                                                       , bench "2" $ whnf (nextBasicPrimal . unEquStd) equStd2
                                                                       , bench "3" $ whnf (nextBasicPrimal . unEquStd) equStd3
                                                                       , bench "4" $ whnf (nextBasicPrimal . unEquStd) equStd4
                                                                       , bench "5" $ whnf (nextBasicPrimal . unEquStd) equStd5
                                                                       ]
                                            ]
                         ]
  ]
