-- vim:set foldmethod=marker:

module SAT.Writer (saveCNF) where
import SAT.Structures
import System.FilePath
import System.IO
-- Writes CNF to a file, does not support XOR clauses

saveCNF :: FilePath -> (Int,CNF) -> IO ()
saveCNF path s = writeFile path $ writeCNF s

writeLiteral :: Literal -> String
writeLiteral (L i) = show i

writeClause :: Clause -> String
writeClause (OR c) = foldl (\s -> \l -> s ++ ' ' : writeLiteral l) "" c ++ " 0"
writeClause (XOR c) = "%" -- indicates error

writeCNF :: (Int,CNF) -> String
writeCNF (n,sat) = hd ++ foldl (\s -> \c -> s ++ writeClause c ++ "\n") "" sat
 where np = length sat
       hd = "p cnf " ++ show n ++ ' ' : show np ++ "\n"

