-- vim:set foldmethod=marker:

module HIPP.Solver (hsolve) where
import HIPP.Structures
import HIPP.Sat
import SAT.Solver
import SAT.Choosers

hsolve_r :: [Genome] -> Int -> ([Haplotype],[(Int,Int)])
hsolve_r g r = if null (fst ret) then hsolve_r g (r+1)
                                 else ret
 where ps  = population_system r g
       vs  = mkVSIDS 1 1 0.5
       sol = solve vs ps
       ret = extract r g sol

hsolve :: [Genome] -> ([Haplotype],[(Int,Int)])
hsolve g = hsolve_r g 1

