-- vim:set foldmethod=marker:

module Solver (dpll,Literal,Clause,CNF,Assum,Decider) where

import           Data.Set          (Set,(\\))
import qualified Data.Set          as S
import           Data.Maybe
import qualified Data.List.Split   as Spl
import           System.IO
import qualified Debug.Trace       as Tr
import qualified Control.Exception as E
import           Data.Typeable     (Typeable)

-- {{{ Utils
type Literal = Int
data Clause = C0 Bool
            | C1 Literal
            | C2 Literal Literal
            | C3 Literal Literal Literal
 deriving (Show)
type CNF = [Clause] -- Empty means true

-- The first set is the one assumed to be true
-- The second set is the one assumed to be false
type Assum = (Set Literal, Set Literal)

type Decider = Set Literal -> (Literal,Bool)

(*:) :: Maybe a -> [a] -> [a]
(*:) Nothing  l = l
(*:) (Just v) l = v:l

-- Splitting on one of m, removing blanks and delimiters
splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn m = Spl.split (Spl.dropBlanks $ Spl.dropDelims $ Spl.oneOf m)

-- Exception
data CNFException = CNFE String deriving (Typeable,Show)
instance E.Exception CNFException

-- }}}

-- {{{ Propagation
-- {{{ Clauses
boundValue :: Literal -> Assum -> Maybe Bool
boundValue l (tr,fl) = if l < 0 then if      S.member (-l) tr then Just False
                                     else if S.member (-l) fl then Just True
                                     else                          Nothing
                       else          if      S.member l tr then Just True
                                     else if S.member l fl then Just False
                                     else                       Nothing

bound :: Literal -> Bool -> Assum
bound l b = if l < 0 then if b then (S.empty, S.singleton (-l))
                               else (S.singleton (-l), S.empty)
            else          if b then (S.singleton l, S.empty)
                               else (S.empty, S.singleton l)

reduceClause :: Clause -> Assum -> Assum
reduceClause (C0 b) _ = (S.empty,S.empty)
reduceClause (C1 l) _ = if l < 0 then (S.empty, S.singleton (-l))
                                 else (S.singleton l, S.empty)
reduceClause (C2 l1 l2) as = if isJust v1 then
                                if not b1 then bound l2 True
                                          else (S.empty,S.empty)
                             else if isJust v2 then
                                if not b2 then bound l1 True
                                          else (S.empty,S.empty)
                             else (S.empty,S.empty)
 where v1 = boundValue l1 as
       v2 = boundValue l2 as
       Just b1 = v1 -- Using laziness, will be accessed only if v1 is just
       Just b2 = v2 -- Idem
reduceClause (C3 l1 l2 l3) as =
    if isJust v1 && isJust v2 then
        if not b1 && not b2 then bound l3 True
                            else (S.empty,S.empty)
    else if isJust v1 && isJust v3 then
        if not b1 && not b3 then bound l2 True
                            else (S.empty,S.empty)
    else if isJust v2 && isJust v3 then
        if not b2 && not b3 then bound l1 True
                            else (S.empty,S.empty)
    else (S.empty,S.empty)
 where (v1,v2,v3) = (boundValue l1 as,boundValue l2 as,boundValue l3 as)
       Just b1 = v1
       Just b2 = v2
       Just b3 = v3
-- }}}

-- {{{ CNF
_propageCNF :: CNF -> Assum -> Assum -> Assum
_propageCNF  []   _  acc     = acc
_propageCNF (h:t) as (at,af) = _propageCNF t as (S.union at ht,S.union af hf)
 where (ht,hf) = reduceClause h as

propageCNF :: CNF -> Assum -> Assum
propageCNF cnf as@(at,af) = if S.null nt && S.null nf
                             then as
                             else propageCNF cnf (S.union at t, S.union af f)
 where (t,f) = _propageCNF cnf as (S.empty,S.empty)
       (nt,nf) = (t \\ at, f \\ af)
-- }}}

-- }}}

-- {{{ DPLL (Davis-Putnam-Logemann-Loveland) algorithm
freeVars :: Int -> Assum -> Set Literal
freeVars n (t,f) = all \\ S.union t f
 where all = S.fromList [1..n]

contradict :: Assum -> Bool
contradict (t,f) = not $ S.null $ S.intersection t f

-- Relies on laziness to compute only what is necessary
_dpll :: CNF -> Int -> Assum -> Decider -> Assum
_dpll cnf n as d =
    if S.null f then as -- If there are no free variable left, the
                        -- assumption is a valid assignement
    else if nval then -- We first set nvar to True before false
        if contradict chkt then chkf else chkt
    else
        if contradict chkf then chkt else chkf
 where (at,af) = as
       f = freeVars n as
       (nvar,nval) = d f -- Choosing which assumption to make
       -- The new assumptions structures
       nast = (S.insert nvar at, af)
       nasf = (at, S.insert nvar af)
       -- The propagation results
       (ptt,pft) = propageCNF cnf nast
       (ptf,pff) = propageCNF cnf nasf
       -- Check recursively
       chkt = _dpll cnf n (S.union at ptt,S.union af pft) d
       chkf = _dpll cnf n (S.union at ptf,S.union af pff) d

-- The dpll algorithm interface
-- Returns Nothing if the CNF is not satisfaisable, and Just a valid
-- assumption on the other case
dpll :: (CNF,Int) -> Decider -> Maybe Assum
dpll (cnf,n) d = if contradict r then Nothing else Just r
 where r = _dpll cnf n (S.empty,S.empty) d
-- }}}

-- {{{ IO
findSize :: CNF -> Int
findSize []    = 0
findSize (c:t) = max mcl $ findSize t
 where mcl = case c of
              C0 _     -> 0
              C1 a     -> abs a
              C2 a b   -> max (abs a) (abs b)
              C3 a b c -> max (abs a) $ max (abs b) (abs c)

parseClause :: String -> Maybe Clause
parseClause str = case n of
                   1 -> Just $ C1 $ read $ head parts
                   2 -> Just $ C2 (read $ head parts)
                                  (read $ head $ tail parts)
                   3 -> Just $ C3 (read $ head parts)
                                  (read $ head $ tail parts)
                                  (read $ head $ tail $ tail parts)
                   _ -> Nothing
 where parts = splitOn " " str
       n = length parts

parseClauses :: [String] -> CNF -> CNF
parseClauses  []   c = c
parseClauses (h:t) c = parseClauses t $ parseClause h *: c

parseDec :: String -> Int
parseDec str = if n < 3 then -1 else read $ parts !! 2
 where parts = splitOn " " str
       n = length parts

parseContent :: [String] -> CNF -> Int -> (CNF,Int)
parseContent []    c n = (c,n)
parseContent (l:t) c n =
 if null l then parseContent t c n else
 case head l of
  'c' -> parseContent t c n
  'p' -> let nb = parseDec l in parseContent t c nb
  _   -> let cls = parseClauses (splitOn "0" l) c in parseContent t cls n

readCNF :: FilePath -> IO (CNF,Int)
readCNF path =
    do file <- openFile path ReadMode
       content <- hGetContents file
       let (cnf,size) = parseContent (lines content) [] (-1)
       if size < 0 then E.throwIO (CNFE "Invalid DIMACS file")
                   else return (cnf,size)
-- }}}

-- {{{ Main loop
decider :: Set Literal -> (Literal,Bool)
decider s = (head $ S.elems s, True)

main :: IO ()
main = do cnf <- readCNF "cnf"
          let r = dpll cnf decider
          if isNothing r then putStrLn "Not satisfaisable"
          else do putStrLn "Satisfaisable :"
                  let Just v = r in putStrLn $ show v
-- }}}

