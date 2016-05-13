-- vim:set foldmethod=marker:

module Main where
import Test.HUnit
import SAT.Status
import SAT.Loader
import SAT.Solver (solve)
import SAT.Choosers
import SAT.Problems

mkAssert :: Chooser a => Bool -> a -> String -> Assertion
mkAssert b c path = do ld <- load path
                       let s = solve () ld
                       if b then if s == Nothing
                          then assertFailure $ "Expected SAT for " ++ path
                          else return ()
                       else if s == Nothing
                          then return ()
                          else assertFailure $ "Expected UNSAT for " ++ path

mkTest :: Chooser a => Bool -> a -> String -> Test
mkTest b c path = TestCase $ mkAssert b c path

tests :: Test
tests = TestList
    [ TestLabel "VSIDS b=1 i=1 a=0.5 chooser" $ TestList
        [ TestLabel "SAT"   $ TestList $ map (mkTest True  vs) satisfiables
        , TestLabel "UNSAT" $ TestList $ map (mkTest False vs) unsatisfiables
        ]
    , TestLabel "() chooser" $ TestList
        [ TestLabel "SAT"   $ TestList $ map (mkTest True  ()) satisfiables
        , TestLabel "UNSAT" $ TestList $ map (mkTest False ()) unsatisfiables
        ]
    ]
 where vs = mkVSIDS 1 1 0.5

main = runTestTT tests

