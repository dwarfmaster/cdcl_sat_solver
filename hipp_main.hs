-- vim:set foldmethod=marker:

module Main where
import Control.Monad
import System.Environment
import System.Exit
import HIPP.Structures
import HIPP.Solver
import HIPP.Sat

compute :: String -> IO ([Haplotype],[(Int,Int)])
compute s = do l <- hload s
               hsolve l

main = do args <- getArgs
          if length args /= 1 then die "Usage : [program] path_to_population"
                              else return ()
          compute (head args) >>= print

