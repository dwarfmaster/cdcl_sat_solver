-- vim:set foldmethod=marker:

module HIPP.Structures (Allele (AZero,AUn), Gene (Monozygote,Heterozygote),
                        Haplotype, Genome, hload, populations)
                        where
import System.FilePath
import System.IO
import qualified Data.List.Split as Spl

-- {{{ Data structures
data Allele = AZero | AUn
data Gene   = Monozygote Allele | Heterozygote
type Haplotype = [Allele]
type Genome    = [Gene]

instance Show Allele where
    show AZero = "0"
    show AUn   = "1"

instance Show Gene where
    show (Monozygote x) = 'G' : show x
    show Heterozygote   = "G2"
-- }}}

-- {{{ Data
populations = [ "hipp/gen" ++ f (show i) | i <- [1..100]]
 where f s = let l = length s in
             if l == 1 then "00" ++ s
             else if l == 2 then "0" ++ s
             else s
-- }}}

-- {{{ Loader
hload :: FilePath -> IO [Genome]
hload path = do file <- openFile path ReadMode
                content <- hGetContents file
                return $ parseLines (lines content)

lineToGenome :: String -> Genome
lineToGenome line = map rd parts
 where parts = filter (not.null) $ Spl.splitOn " " line
       rd c = if      c == "0" then Monozygote AZero
              else if c == "1" then Monozygote AUn
              else                  Heterozygote

parseLines :: [String] -> [Genome]
parseLines = map lineToGenome
-- }}}

