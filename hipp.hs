-- vim:set foldmethod=marker:

module HIPP.Structures where

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

-- {{{ Example
population :: [Genome]
population = [ [ Monozygote AZero, Monozygote AUn]
             , [ Heterozygote,     Monozygote AZero]
             , [ Monozygote AUn,   Heterozygote]
             , [ Heterozygote,     Heterozygote]
             ]
-- }}}

-- TODO loader

