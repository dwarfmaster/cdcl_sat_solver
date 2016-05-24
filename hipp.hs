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
population = [ [ Monozygote AZero, Monozygote AZero
               , Monozygote AZero, Monozygote AUn ]
             , [ Monozygote AUn,   Monozygote AZero
               , Monozygote AZero, Monozygote AUn]
             , [ Monozygote AZero, Monozygote AUn
               , Monozygote AZero, Monozygote AUn]
             , [ Heterozygote,     Monozygote AZero
               , Monozygote AZero, Monozygote AUn]
             , [ Monozygote AZero, Heterozygote
               , Monozygote AZero, Monozygote AUn]
             , [ Heterozygote,     Heterozygote
               , Monozygote AZero, Monozygote AUn ]
             ]

poptest :: [Genome]
poptest = [ [ Monozygote AZero, Monozygote AUn ]
          , [ Monozygote AUn,   Heterozygote   ]
          , [ Heterozygote,     Monozygote AUn ]
          ]
-- }}}

-- TODO loader

