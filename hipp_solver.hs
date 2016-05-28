-- vim:set foldmethod=marker:

module HIPP.Solver (hsolve) where
import HIPP.Structures
import HIPP.Sat
import SAT.Solver
import SAT.Choosers
import SAT.Writer
import SAT.Structures
import Data.Vector (Vector,fromList)
import qualified Data.List.Split as Spl
import System.FilePath
import System.IO
import System.Process

minisat :: FilePath
minisat = "minisat/core/minisat_static"

tmpFileIn :: FilePath
tmpFileIn = "/tmp/minisat_in"
tmpFileOut :: FilePath
tmpFileOut = "/tmp/minisat_out"

minisat_cmd :: String
minisat_cmd = minisat ++ " -verb=0 " ++ tmpFileIn ++ " " ++ tmpFileOut

readOutLine :: String -> Vector Bool
readOutLine ln = fromList $ map ((>=0) . read) parts
 where parts = filter (not.null) $ Spl.splitOn " " ln

readOut :: FilePath -> IO (Maybe (Vector Bool))
readOut path = do
    f <- openFile path ReadMode
    c <- hGetContents f
    let lns = lines c
    r <- if head lns == "UNSAT" then return Nothing
    else return $ Just $ readOutLine $ head $ tail lns
    hClose f
    return r

minisat_solve :: (Int,CNF) -> IO (Maybe (Vector Bool))
minisat_solve (n,sat) = do
    saveCNF tmpFileIn (n,sat)
    system minisat_cmd
    readOut tmpFileOut

hsolve_r :: [Genome] -> Int -> IO ([Haplotype],[(Int,Int)])
hsolve_r g r = do
    sol <- minisat_solve ps
    if sol == Nothing then hsolve_r g (r+1)
    else return $ extract r g sol
 where ps  = population_system r g

hsolve :: [Genome] -> IO ([Haplotype],[(Int,Int)])
hsolve g = hsolve_r g 1

