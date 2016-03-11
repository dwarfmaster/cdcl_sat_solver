-- vim:set foldmethod=marker:

import Data.Set
import Data.Maybe

-- {{{ Utils
type Literal = Int
data Clause = C0 Bool
            | C1 Literal
            | C2 Literal Literal
            | C3 Literal Literal Literal
type CNF = [Clause] -- Empty means true

-- The first set is the one assumed to be true
-- The second set is the one assumed to be false
type Assum = (Set Literal, Set Literal)

type Decider = Set Literal -> (Literal,Bool)

isEmptySet :: Set a -> Bool
isEmptySet s = size s == 0

-- }}}

-- {{{ Propagation
-- {{{ Clauses
boundValue :: Literal -> Assum -> Maybe Bool
boundValue l (tr,fl) = if l < 0 then if      member (-l) tr then Just False
                                     else if member (-l) fl then Just True
                                     else                        Nothing
                       else          if      member l tr then Just True
                                     else if member l fl then Just False
                                     else                     Nothing

bound :: Literal -> Bool -> Assum
bound l b = if l < 0 then if b then (empty, singleton (-l))
                               else (singleton (-l), empty)
            else          if b then (singleton l, empty)
                               else (empty, singleton l)

reduceClause :: Clause -> Assum -> Assum
reduceClause (C0 b) _ = (empty,empty)
reduceClause (C1 l) _ = if l < 0 then (empty, singleton (-l))
                                 else (singleton l, empty)
reduceClause (C2 l1 l2) as = if isJust v1 then
                                if not b1 then bound l2 True
                                          else (empty,empty)
                             else if isJust v2 then
                                if not b2 then bound l1 True
                                          else (empty,empty)
                             else (empty,empty)
 where v1 = boundValue l1 as
       v2 = boundValue l2 as
       Just b1 = v1 -- Using laziness, will be accessed only if v1 is just
       Just b2 = v2 -- Idem
reduceClause (C3 l1 l2 l3) as =
    if isJust v1 && isJust v2 then
        if not b1 && not b2 then bound l3 True
                            else (empty,empty)
    else if isJust v1 && isJust v3 then
        if not b1 && not b3 then bound l2 True
                            else (empty,empty)
    else if isJust v2 && isJust v3 then
        if not b2 && not b3 then bound l1 True
                            else (empty,empty)
    else (empty,empty)
 where (v1,v2,v3) = (boundValue l1 as,boundValue l2 as,boundValue l3 as)
       (Just b1,Just b2,Just b3) = (v1,v2,v3) -- as above
-- }}}

-- {{{ CNF
_propageCNF :: CNF -> Assum -> Assum -> Assum
_propageCNF  []   _  acc     = acc
_propageCNF (h:t) as (at,af) = (union at ht,union af hf)
 where (ht,hf) = reduceClause h as

-- TODO propagate recursively
propageCNF :: CNF -> Assum -> Assum
propageCNF cnf as = _propageCNF cnf as (empty,empty)
-- }}}

-- }}}

-- {{{ DPLL (Davis-Putnam-Logemann-Loveland) algorithm
freeVars :: Int -> Assum -> Set Literal
freeVars n (t,f) = all \\ union t f
 where all = fromList [1..n]

contradict :: Assum -> Bool
contradict (t,f) = not $ isEmptySet $ intersection t f

-- Relies on laziness to compute only what is necessary
_dpll :: CNF -> Int -> Assum -> Decider -> Assum
_dpll cnf n as d =
    if isEmptySet f then as -- If there are no free variable left, the
                            -- assumption is a valid assignement
    else if nval then -- We first set nvar to True before false
        if contradict chkt then chkf else chkt
    else
        if contradict chkf then chkt else chkf
 where (at,af) = as
       f = freeVars n as
       (nvar,nval) = d f -- Choosing which assumption to make
       -- The new assumptions structures
       nast = (insert nvar at, af)
       nasf = (at, insert nvar af)
       -- The propagation results
       (ptt,pft) = propageCNF cnf nast
       (ptf,pff) = propageCNF cnf nasf
       -- Check recursively
       chkt = _dpll cnf n (union at ptt,union af pft) d
       chkf = _dpll cnf n (union at ptf,union af pff) d

-- The dpll algorithm interface
-- Returns Nothing if the CNF is not satisfaisable, and Just a valid
-- assumption on the other case
dpll :: (CNF,Int) -> Decider -> Maybe Assum
dpll (cnf,n) d = if contradict r then Nothing else Just r
 where r = _dpll cnf n (empty,empty) d
-- }}}

