-- vim:set foldmethod=marker:

module HIPP.Sat (population_system,extract) where
import HIPP.Structures
import SAT.Structures
import Data.Vector ((!))
import qualified Data.Vector as V
import System.IO
import Control.Monad.Loops
import Control.Monad

-- {{{ HIPP -> SAT
population_system
    :: Int       -- number of haplotypes
    -> [Genome]  -- the population (all genomes must have same size)
    -> Handle    -- the file to write to
    -> IO ()
population_system r p h = do--swp $ foldl (genome_system r n m) (ds,mx) $ zip p [1..]
    at_least r n m h
    foldM (genome_system r n m h) mx $ zip p [1..]
    return ()
 where m  = length $ head p
       n  = length p
       mx = 2 * r * n + r * m

-- For each genome parent, exactly one haplotype must be selected
at_least :: Int -> Int -> Int -> Handle -> IO ()
at_least r n m fl = mapM_ writeC [1..n]
 where writeC i = do mapM_ (\k -> hPutStr fl (show $ s_a (r,n,m) k i True)
                            >> hPutChar fl ' ')
                           [1..r]
                     hPutStr fl "0\n"
                     mapM_ (\k -> hPutStr fl (show $ s_b (r,n,m) k i True)
                            >> hPutChar fl ' ')
                           [1..r]
                     hPutStr fl "0\n"


genome_system
    :: Int          -- number of haplotypes
    -> Int          -- Size of population
    -> Int          -- size of genotype
    -> Handle
    -> Int          -- actual number of variables
    -> (Genome,Int) -- genome to encode and position
    -> IO Int       -- new number of variables
genome_system r np m h nv (g,j) = foldM (gene_system r np m j h) nv
                                $ zip g [1..]

gene_system :: Int -> Int -> Int
            -> Int -- number of genome
            -> Handle
            -> Int
            -> (Gene,Int)
            -> IO Int
gene_system r np m j fl nv (Monozygote AZero,i) = do
    forM_ [1..r] writeC
    return nv
 where writeC k = do hPutStr fl $ show $ h (r,np,m) k i False
                     hPutStr fl $ (:) ' ' $ show $ s_a (r,np,m) k j False
                     hPutStr fl " 0\n"
                     hPutStr fl $ show $ h (r,np,m) k i False
                     hPutStr fl $ (:) ' ' $ show $ s_b (r,np,m) k j False
                     hPutStr fl " 0\n"
gene_system r np m j fl nv (Monozygote AUn,i)   = do
    forM_ [1..r] writeC
    return nv
 where writeC k = do hPutStr fl $ show $ h (r,np,m) k i True
                     hPutStr fl $ (:) ' ' $ show $ s_a (r,np,m) k j False
                     hPutStr fl " 0\n"
                     hPutStr fl $ show $ h (r,np,m) k i True
                     hPutStr fl $ (:) ' ' $ show $ s_b (r,np,m) k j False
                     hPutStr fl " 0\n"
gene_system r np m j fl nv (Heterozygote,i)     = do
    hPutStr fl $ show g1 ++ ' ' : (show g2) ++ " 0\n"
    hPutStr fl $ show ng1 ++ ' ' : (show ng2) ++ " 0\n"
    forM_ [1..r] writeC
    return $ nv + 2
 where g1  = nv + 1
       ng1 = - (nv + 1)
       g2  = nv + 2
       ng2 = - (nv + 2)
       writeC k = do
           hPutStr fl $ show $ h (r,np,m) k i True
           hPutStr fl $ (:) ' ' $ show $ s_a (r,np,m) k j False
           hPutStr fl $ (:) ' ' $ show g1
           hPutStr fl " 0\n"
           hPutStr fl $ show $ h (r,np,m) k i False
           hPutStr fl $ (:) ' ' $ show $ s_a (r,np,m) k j False
           hPutStr fl $ (:) ' ' $ show ng1
           hPutStr fl " 0\n"
           hPutStr fl $ show $ h (r,np,m) k i True
           hPutStr fl $ (:) ' ' $ show $ s_b (r,np,m) k j False
           hPutStr fl $ (:) ' ' $ show g2
           hPutStr fl " 0\n"
           hPutStr fl $ show $ h (r,np,m) k i False
           hPutStr fl $ (:) ' ' $ show $ s_b (r,np,m) k j False
           hPutStr fl $ (:) ' ' $ show ng2
           hPutStr fl " 0\n"

s :: Bool          -- s_a (True) or s_b (False)
  -> (Int,Int,Int) -- number of haplotypes, size of population, size of genome
  -> Int           -- id of haplotype
  -> Int           -- id of genome in population
  -> Bool          -- sgn of literal
  -> Int
s a (r,np,m) k i b = if b then n else (-n)
 where d = r*m
       l = if a then 0 else 1
       n = d + 2 * ((k-1)*np + i) + l - 1

s_a = s True
s_b = s False

h :: (Int,Int,Int) -- number of haplotypes, size of population, size of genome
  -> Int           -- id of haplotype
  -> Int           -- position in genome
  -> Bool          -- sgn of literal
  -> Int
h (r,np,m) k i b = if b then n else (-n)
 where n = (k-1)*m + i
-- }}}

-- {{{ SAT -> HIPP
extract
    :: Int -- number of haplotypes
    -> [Genome]
    -> Maybe (V.Vector Bool)
    -> ([Haplotype], [(Int,Int)]) -- The haplotypes used for each genome
extract _ _ Nothing  = ([], [])
extract r g (Just s) = ( foldl eh [] (reverse [1..r])
                       , foldl fh [] (reverse [1..np]))
 where eh :: [Haplotype] -> Int -> [Haplotype]
       eh l i = get (r,np,m) s i : l
       np = length g
       m  = length $ head g
       fh :: [(Int,Int)] -> Int -> [(Int,Int)]
       fh l i = getP (r,np,m) s i : l

get :: (Int,Int,Int) -> V.Vector Bool -> Int -> Haplotype
get (r,np,m) s i = map bta $ foldl (\l -> \j -> s!(id i j) : l) [] l
 where id a b = h (r,np,m) a b True
       bta True  = AUn
       bta False = AZero
       l = reverse [1..m] 

getP :: (Int,Int,Int) -> V.Vector Bool -> Int -> (Int,Int)
getP (r,np,m) sol i = (getSa 1, getSb 1)
 where d b j = s b (r,np,m) j i True
       getSa j = if sol!(d True j)  then j else getSa (j+1)
       getSb j = if sol!(d False j) then j else getSb (j+1)

-- }}}

