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
    [ TestLabel "() chooser" $ TestList
        [ TestLabel "SAT"   $ TestList $ map (mkTest True ())  satisfiables
        , TestLabel "UNSAT" $ TestList $ map (mkTest False ()) unsatisfiables
        ]
    ]

main = runTestTT tests

