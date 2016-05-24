-- vim:set foldmethod=marker:

module HIPP.Sat (population_system,extract) where
import HIPP.Structures
import SAT.Structures
import Data.Array

-- {{{ HIPP -> SAT
population_system
    :: Int       -- number of haplotypes
    -> [Genome]  -- the population (all genomes must have same size)
    -> (CNF,Int) -- the SAT instance with number of variables
population_system r p = foldl (genome_system r n m) (ds,mx) $ zip p [1..]
 where m  = length $ head p
       n  = length p
       mx = 2 * r * n + r * m
       ds = at_least r n m

-- For each genome parent, exactly one haplotype must be selected
at_least :: Int -> Int -> Int -> CNF
at_least r n m = foldl bd [] [1..n]
 where bd st i = XOR [s_a (r,n,m) k i True | k <- [1..r]]
               : XOR [s_b (r,n,m) k i True | k <- [1..r]]
               : st

genome_system
    :: Int          -- number of haplotypes
    -> Int          -- Size of population
    -> Int          -- size of genotype
    -> (CNF,Int)    -- actual SAT instance and size
    -> (Genome,Int) -- genome to encode and position
    -> (CNF,Int)
genome_system r np m (sat,n) (g,j) = foldl (gene_system r np m j) (sat,n)
                                   $ zip g [1..]

gene_system :: Int -> Int -> Int
            -> Int -- number of genome
            -> (CNF,Int)
            -> (Gene,Int)
            -> (CNF,Int)
gene_system r np m j (sat,n) (Monozygote AZero,i) =
    (foldl bd sat [1..r], n)
 where bd st k = OR [h (r,np,m) k i False, s_a (r,np,m) k j False]
               : OR [h (r,np,m) k i False, s_b (r,np,m) k j False]
               : st
gene_system r np m j (sat,n) (Monozygote AUn,i)   =
    (foldl bd sat [1..r], n)
 where bd st k = OR [h (r,np,m) k i True, s_a (r,np,m) k j False]
               : OR [h (r,np,m) k i True, s_b (r,np,m) k j False]
               : st
gene_system r np m j (sat,n) (Heterozygote,i)     =
    (XOR [g1,g2] : foldl bd sat [1..r], n+2)
 where g1  = L $ n + 1
       ng1 = L $ - (n + 1)
       g2  = L $ n + 2
       ng2 = L $ - (n + 2)
       bd st k = OR [h (r,np,m) k i True,  s_a (r,np,m) k j False, g1]
               : OR [h (r,np,m) k i False, s_a (r,np,m) k j False, ng1]
               : OR [h (r,np,m) k i True,  s_b (r,np,m) k j False, g2]
               : OR [h (r,np,m) k i False, s_b (r,np,m) k j False, ng2]
               : st

s :: Bool          -- s_a (True) or s_b (False)
  -> (Int,Int,Int) -- number of haplotypes, size of population, size of genome
  -> Int           -- id of haplotype
  -> Int           -- id of genome in population
  -> Bool          -- sgn of literal
  -> Literal
s a (r,np,m) k i b = if b then L n else L $ -n
 where d = r*m
       l = if a then 0 else 1
       n = d + 2 * ((k-1)*np + i) + l - 1

s_a = s True
s_b = s False

h :: (Int,Int,Int) -- number of haplotypes, size of population, size of genome
  -> Int           -- id of haplotype
  -> Int           -- position in genome
  -> Bool          -- sgn of literal
  -> Literal
h (r,np,m) k i b = if b then L n else L $ -n
 where n = (k-1)*m + i
-- }}}

-- {{{ SAT -> HIPP
extract
    :: Int -- number of haplotypes
    -> [Genome]
    -> Maybe (Array Literal Bool)
    -> ([Haplotype], [(Int,Int)]) -- The haplotypes used for each genome
extract _ _ Nothing  = ([], [])
extract r g (Just s) = ( foldl eh [] (reverse [1..r])
                       , foldl fh [] (reverse [1..r]))
 where eh :: [Haplotype] -> Int -> [Haplotype]
       eh l i = get (r,np,m) s i : l
       np = length g
       m  = length $ head g
       fh :: [(Int,Int)] -> Int -> [(Int,Int)]
       fh l i = getP (r,np,m) s i : l

get :: (Int,Int,Int) -> Array Literal Bool -> Int -> Haplotype
get (r,np,m) s i = map bta $ foldl (\l -> \j -> s!(id i j) : l) [] l
 where id a b = h (r,np,m) a b True
       bta True  = AUn
       bta False = AZero
       l = reverse [1..m] 

getP :: (Int,Int,Int) -> Array Literal Bool -> Int -> (Int,Int)
getP (r,np,m) sol i = (getSa 1, getSb 1)
 where d b j = s b (r,np,m) j i True
       getSa j = if sol!(d True j)  then j else getSa (j+1)
       getSb j = if sol!(d False j) then j else getSb (j+1)

-- }}}

