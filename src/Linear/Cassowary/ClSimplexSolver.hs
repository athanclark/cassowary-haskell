module Linear.Cassowary.ClSimplexSolver where



addConstraint :: ClSimplexSolver -> ClConstraint -> IO ()

removeConstraint :: ClSimplexSolver -> ClConstraint -> IO ()

addEditVar :: ClSimplexSolver -> ClVariable -> ClStrength -> IO ()

removeEditVar :: ClSimplexSolver -> ClVariable -> IO ()

beginEdit :: ClSimplexSolver -> IO ()

suggestValue :: ClSimplexSolver -> ClVariable -> Double -> IO ()

endEdit :: ClSimplexSolver -> IO ()

resolve :: ClSimplexSolver -> IO ()

-- addPointStays :: ClSimplexSolver -> [(ClVariable, ClVariable)] -> IO () ?

setAutoSolve :: ClSimplexSolver -> Boolean -> IO ()

isAutoSolving :: ClSimplexSolver -> IO Boolean

solve :: ClSimplexSolver -> IO ()

reset :: ClSimplexSolver -> IO ()
