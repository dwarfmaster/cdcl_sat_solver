-- vim:set foldmethod=marker:

module Main where
import Criterion.Main
import System.Directory
import SAT.Status
import SAT.Loader
import SAT.Solver (solve)
import SAT.Choosers
import SAT.Problems

run :: Chooser a => a -> String -> IO ()
run c path = do ld <- load path
                return $ solve c ld
                return ()

test :: Chooser a => a -> String -> Benchmark
test c s = bench s $ nfIO $ run c s

main = defaultMain
    [ bgroup "SAT () chooser - " $ map (test ()) satisfiables
    , bgroup "UNSAT () chooser - " $ map (test ()) unsatisfiables
    , bgroup "SAT (VSIDS b=1 i=1 a=.5) chooser - "
        $ map (test vs) satisfiables
    , bgroup "UNSAT (VSIDS b=1 i=1 a=.5) chooser - "
        $ map (test vs) unsatisfiables
    ]
 where vs = mkVSIDS 1 1 0.5

