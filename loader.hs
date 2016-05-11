-- vim:set foldmethod=marker:

module SAT.Loader (load) where
import System.FilePath
import System.IO
import qualified Data.List.Split as Spl
import Control.Exception
import Data.Typeable (Typeable)
import SAT.Structures

data CNFException = CNFE String deriving (Typeable,Show)
instance Exception CNFException

load :: FilePath -> IO (Int,CNF)
load path = do file <- openFile path ReadMode
               content <- hGetContents file
               let (size,cnf) = parseLines (lines content)
               if size < 0 then throwIO $ CNFE "Invalid DIMACS File"
                           else return (size,cnf)

findSize :: CNF -> Int
findSize cnf = maximum $ map mcl cnf
 where mcl = maximum . (map abl)
       abl (L l) = abs l

parseClause :: String -> (Int,Clause)
parseClause str = (0, filter (\(L l) -> l /= 0) $ map rlit parts)
 where parts = filter (not.null) $ Spl.splitOn " " str
       rlit s = L (read s :: Int)

-- Concat only if not empty
(*:) :: (Int,[a]) -> (Int,[[a]]) -> (Int,[[a]])
(n,[]) *: (n2,l) = if n <= 0 then (n2,l)   else (n,l) 
(n,h)  *: (n2,l) = if n <= 0 then (n2,h:l) else (n,h:l)

parseDesc :: String -> (Int,Clause)
parseDesc str = (nb,[])
 where parts = filter (not.null) $ Spl.splitOn " " str
       n = length parts
       nb = if n == 4 then read $ parts !! 2 else 0

firstChar :: String -> Char
firstChar []    = 'c'
firstChar (c:_) = c

parseLine :: String -> (Int,Clause)
parseLine s = if c == 'p'      then parseDesc s
              else if c /= 'c' then parseClause s
              else                  (0,[])
 where c = firstChar s

parseLines :: [String] -> (Int,CNF)
parseLines lines = foldr (*:) (0,[]) $ map parseLine lines

