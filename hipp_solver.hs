-- vim:set foldmethod=marker:

module HIPP.Solver (hsolve) where
import HIPP.Structures
import HIPP.Sat
import SAT.Solver
import SAT.Choosers

hsolve_r :: [Genome] -> Int -> [Haplotype]
hsolve_r g r = if null ret then hsolve_r g (r+1)
                           else ret
 where (s,n) = population_system r g
       vs    = mkVSIDS 1 1 0.5
       sol   = solve vs (n,s)
       ret   = extract r g sol

hsolve :: [Genome] -> [Haplotype]
hsolve g = hsolve_r g 1

