-- vim:set foldmethod=marker:

module Main where
import Control.Monad
import System.TimeIt
import HIPP.Structures
import HIPP.Solver
import HIPP.Sat

compute :: String -> IO ([Haplotype],[(Int,Int)])
compute s = do l <- hload s
               return $ hsolve l

computeAll :: IO [([Haplotype],[(Int,Int)])]
computeAll = forM populations compute

main = timeIt $ compute "hipp/gen065" >>= print

