-- vim:set foldmethod=marker:
-- Haplotype inference by pure parsimony

import Solver

-- {{{ Utils
type Haplotype  = [Bool]
data Allele     = Heterozygote | Homozygote Bool
type Genotype   = [Allele]
type Population = [Genotype]
-- }}}

-- {{{ Population reading
-- TODO
-- }}}

-- {{{ SAT-making
-- TODO
-- }}}

