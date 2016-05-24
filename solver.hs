-- vim:set foldmethod=marker:

module SAT.Solver (solve) where
import Data.Array (Array,(!),(//)
                   ,Ix,range,index,inRange)
import qualified Data.Array as A
import Data.Maybe
import qualified Data.Sequence as S
import Data.Sequence ((<|), ViewR((:>)))
import Control.Monad.ST
import Control.Monad.Loops
import qualified Data.Array.ST as SA
import Data.STRef

import SAT.Structures
import SAT.Status

-- {{{ Data structures
data MdSt a b = MdSt (Status a -> (Status a,b))
instance Functor (MdSt a) where
    fmap f (MdSt g) = MdSt (\s -> let (ns,x) = g s in (ns,f x))
instance Applicative (MdSt a) where
    pure x = MdSt (\s -> (s,x))
    (<*>) (MdSt f) (MdSt a) = MdSt (\s -> let (s2,g) = f s  in
                                          let (s3,x) = a s2 in
                                          (s3, g x))
instance Monad (MdSt a) where
    (>>=) (MdSt f) b = MdSt (\s -> let (ns,x) = f s in
                                   let MdSt g = b x in g ns)
    return = pure

runMdSt :: Int -> CNF -> a -> MdSt a b -> b
runMdSt n sat c (MdSt m) = s `seq` x -- Makes sure the state is executed
                                     -- /!\ for debug purpose only
 where (s,x) = m $ mkStatus n sat c

-- }}}

-- {{{ Tools
mapArray :: Ix i => (a -> b) -> Array i a -> Array i b
mapArray f a = A.array b [(i,f $ a ! i) | i <- range b]
 where b = A.bounds a

neg :: Literal -> Literal
neg (L l) = L $ -l

sgn :: Literal -> Bool
sgn (L l) = if l < 0 then False else True

choose :: Chooser a => MdSt a (Maybe Literal)
choose = MdSt $ \s -> let (a,l) = ch_choose s in (s {chooser_st = a}, l)

init_chooser :: Chooser a => MdSt a ()
init_chooser = MdSt $ \s -> let a = ch_init s in (s {chooser_st = a}, ())

mult :: Literal -> MBool -> MBool
mult _     Nothing  = Nothing
mult (L l) (Just b) = if l < 0 then Just $ not b else Just b

status :: Literal -> MdSt a MBool
status l = MdSt $ \s -> (s,mult l $ (head $ vars_st s) ! l)

_bind :: Literal -> Clause -> Literal -> MdSt a ()
_bind l c lv = MdSt $ \s -> let h:t = vars_st s in let hb:tb = bound_st s in
                            let ls = level_st s in
                            (s {vars_st = h // [(l,Just $ sgn l)] : t
                               ,bound_st = hb // [(l,c)] : tb
                               ,level_st = ls // [(l,lv)]}
                            , ())

bind :: Literal -> Clause -> Literal -> MdSt a ()
bind l c lv = do s <- status l
                 if s == Nothing then _bind l c lv
                 else if s /= Just True then launch_error c
                 else return ()

push :: MdSt a ()
push = MdSt $ \s -> let v = vars_st s in let b = bound_st s in
                    (s {vars_st = head v : v, bound_st = head b : b}, ())

pop :: MdSt a ()
pop = MdSt $ \s -> let v = vars_st s in let b = bound_st s in
                   (s {vars_st = tail v, bound_st = tail b}, ())

-- Can only be called if vars_st s has at least two elements
collapse :: MdSt a ()
collapse = MdSt $ \s -> let h1:h2:t = vars_st s in let b1:b2:bt = bound_st s in
                        (s {vars_st = h1:t, bound_st = b1:bt}, ())

clear_vars :: MdSt a ()
clear_vars = MdSt $ \s -> let b = A.bounds $ head $ vars_st s in
                    (s {vars_st = A.array b [(i,Nothing) | i <- range b] : []}
                    , ())

sat_get :: MdSt a CNF
sat_get = MdSt $ \s -> (s,sat_st s)

sat_set :: CNF -> MdSt a ()
sat_set sat = MdSt $ \s -> (s {sat_st = sat}, ())

sat_add_clause :: Chooser a => Clause -> MdSt a ()
sat_add_clause c = MdSt $ \s -> let na = ch_conflit s c in
                                (s { sat_st     = c : sat_st s
                                   , new_st     = c
                                   , chooser_st = na }
                                , ())

-- Check if the backtracking should be stopped, assuming pop has been called
-- when necessary
stop_back :: Literal -> MdSt a Bool
stop_back lv = do n <- new
                  if not (isOR n) then return True
                  else do let (OR n2) = n
                          r <- mapM nf n2
                          return $ foldl (&&) True r
 where new = MdSt $ \s -> (s,new_st s)
       nf l = do llv <- level l
                 -- Equality independant of sign
                 return $ lv == llv
       level l = MdSt $ \s -> (s,level_st s ! l)

clear_new :: MdSt a ()
clear_new = MdSt $ \s -> (s {new_st = CEmpty}, ())

-- Test if restart is necessary
should_restart :: MdSt a Bool
should_restart = MdSt $ \s -> (s, restart_st s <= 0)

-- Decrement the restart counter
dec_restart :: MdSt a ()
dec_restart = MdSt $ \s -> let n = restart_st s in
                           if n == 0 then (s, ())
                                     else (s {restart_st = n - 1}, ())

is_error :: MdSt a Bool
is_error = MdSt $ \s -> (s,(/= CEmpty) $ error_st s)

launch_error :: Clause -> MdSt a ()
launch_error c = MdSt $ \s -> (s {error_st = c}, ())

clear_error :: MdSt a ()
clear_error = MdSt $ \s -> (s {error_st = CEmpty}, ())

get_error :: MdSt a (Clause)
get_error = MdSt $ \s -> (s, error_st s)

-- Return and remove a literal to bound
tobnd_get :: MdSt a (Maybe (Literal,Clause,Literal))
tobnd_get = MdSt $ \s -> case tobnd_st s of
                         []  -> (s, Nothing)
                         h:t -> (s {tobnd_st = t}, Just h)

tobnd_peek :: MdSt a (Maybe (Literal,Clause,Literal))
tobnd_peek = MdSt $ \s -> (s, case tobnd_st s of
                              []  -> Nothing
                              h:_ -> Just h)

tobnd_add :: Literal -> Clause -> Literal -> MdSt a ()
tobnd_add h c lv = MdSt $ \s -> (s {tobnd_st = (h,c,lv) : tobnd_st s}, ())

clear_tobnd :: MdSt a ()
clear_tobnd = MdSt $ \s -> (s {tobnd_st = []}, ())

isOR :: Clause -> Bool
isOR (OR _) = True
isOR _      = False

-- Create a clause for the contradiction arising in the parameter clause
_derive_clause :: Clause -> Status a -> Clause
_derive_clause (OR c) s = runST $ do
    arr <- SA.newArray (L 1,n) False :: ST s (SA.STArray s Literal Bool)
    q <- newSTRef S.empty
    r <- newSTRef []
    sequence_ $ map (\x -> modifySTRef' q (x <|)) c
    whileM_ ((fmap (not . null) . readSTRef) q) $ do
        qr <- readSTRef q
        let qt:>l = S.viewr qr
        writeSTRef q qt
        b <- SA.readArray arr l
        if b then return ()
        else do
            SA.writeArray arr l True
            let bd = bnd ! l
            if isOR bd
                then let (OR b) = bd in
                     sequence_ $ map (\x -> modifySTRef' q (x <|)) b
                else modifySTRef' r (\x -> l : x)
    rc <- readSTRef r
    return $ OR rc
 where sat = sat_st s
       bnd = head $ bound_st s
       v   = head $ vars_st s
       n   = snd $ A.bounds v
_derive_clause _ _ = CEmpty

derive_clause :: Clause -> MdSt a Clause
derive_clause c = MdSt $ \s -> (s, _derive_clause c s)

-- Take the first element of l for which f is true, and put it first
-- Do nothing if f is always false on l
raise_on :: (a -> MdSt b Bool) -> [a] -> MdSt b [a]
raise_on f l = do (r,t) <- raise_r f l
                  if isNothing r then return l
                  else let (Just h) = r in return $ h : t
 where raise_r :: (a -> MdSt b Bool) -> [a] -> MdSt b (Maybe a, [a])
       raise_r f [] = return (Nothing,[])
       raise_r f (h:t) = do b <- f h
                            if b then return (Just h, t)
                            else do (r,t2) <- raise_r f t
                                    return (r,h:t2)

-- }}}

-- {{{ Control
-- Assums u will return true before the list empties
tryuntil :: (c -> MdSt b Bool) -> (a -> MdSt b c) -> [a] -> MdSt b c
tryuntil u f (h:t) = do r <- f h
                        b <- u r
                        if b then return r
                             else tryuntil u f t

formap :: (a -> MdSt b c) -> [a] -> MdSt b [c]
formap f []    = return []
formap f (h:t) = do c <- f h
                    l <- formap f t
                    return $ c : l

while :: MdSt a Bool -> MdSt a () -> MdSt a ()
while f g = do b <- f
               if b then g >> while f g else return ()

-- }}}

-- {{{ CDCL
-- Apply two-watch simplification to a clause
two_watch :: Literal -> Clause -> MdSt a Clause
two_watch _  CEmpty      = return CEmpty
two_watch lv (OR (h:[])) = -- Shouldn't happen, as a preprocessor should have
                        -- simplified singles clauses
    do b <- status h
       if b == Just False then launch_error $ OR [h]
       else if b == Nothing then tobnd_add h (OR [h]) lv
       else return ()
       return $ OR [h]
two_watch lv (OR c) = -- Here c has at least two elements
    do nc <- do (h:t) <- raise_on r $ c -- We make sure the first variable is
                                        -- not bound to false
                s <- status h
                -- Idem for the second one
                t2 <- if s == Just True then return t
                                        else raise_on r t
                return $ h : t2
       let l1:l2:_ = nc -- Main idea of two_watch : we only check the two first
                        -- literals of each clause
       s1 <- status l1
       s2 <- status l2
       if s1 == Just False then launch_error $ OR c -- Means all variables are
                                                    -- bound to false
       -- Means all variables are bound to false except the first one, which
       -- is not bound and thus must be
       else if s2 == Just False && s1 == Nothing then tobnd_add l1 (OR c) lv
       else return ()
       return $ OR nc
 where r l = do b <- status l
                return $ b /= (Just False)
two_watch lv (XOR (h:[])) =
    do b <- status h
       if b == Just False then launch_error $ XOR [h]
       else if b == Nothing then tobnd_add h (XOR [h]) lv
       else return ()
       return $ XOR [h]
two_watch lv (XOR c) =
    do (h:t) <- raise_on ist c
       s <- status h
       if s == Just True
       then do (h2:t2) <- raise_on ist t
               s2 <- status h2
               if s2 == Just True then do launch_error $ XOR c
                                          return $ XOR (h:h2:t2)
               else do mapM_ mp c
                       return $ XOR (h:h2:t2)
       else do (h2:t2) <- raise_on isn c
               s2 <- status h2
               if s2 == Just False then do launch_error $ XOR c
                                           return $ XOR (h2:t2)
               else do (h3:t3) <- raise_on isn t2
                       s3 <- status h3
                       if s3 == Nothing then return (XOR (h2:h3:t3))
                       else do tobnd_add h2 (XOR c) lv
                               return $ XOR (h2:h3:t3)
 where ist l = do b <- status l
                  return $ b == Just True
       isn l = do b <- status l
                  return $ b == Nothing
       mp l = do s <- status l
                 if s == Nothing then tobnd_add (neg l) (XOR c) lv
                 else return ()

-- Apply two-watch simplification to all clauses, bounding all necessary
-- variables
two_watch_all :: MdSt a ()
two_watch_all = do
    while test $ do
        tb <- tobnd_get
        let (l,c,lv) = fromJust tb
        -- We try to bind the next variable which value is fixed
        bind l c lv
        -- If it resulted in an error, we abort for the error to be treated
        -- in cdcl
        e <- is_error
        if e then return ()
        -- If it could be bound, we launch the propagation through all clauses
        -- and update the sat representation
        else do cnf <- sat_get
                ncnf <- formap (two_watch lv) cnf
                sat_set ncnf
                return ()
    clear_tobnd
 where test = do e  <- is_error
                 tb <- tobnd_peek
                 return $ not e && tb /= Nothing

-- The cdcl algorithm, ending on restarts
-- Returns Nothing if it ended due to a restart, Just True if the problem is
-- SAT and Just False if UNSAT
cdcl :: Chooser a => MdSt a (Maybe Bool)
cdcl = do e <- is_error
          -- If conflict reached, either restart or learn clause and backtrack
          if e then do dec_restart
                       b <- should_restart
                       if b then return Nothing
                            else do dc <- get_error >>= derive_clause
                                    if isOR dc then sat_add_clause dc
                                               else return ()
                                    return $ Just False
          -- If there is no conflict, choose a variable
          else do ml <- choose
                  -- If all the variables are already bound, the assignement
                  -- is a solution as there is no error
                  if ml == Nothing then return $ Just True
                  -- On the other case, bound the new variable, propagate and
                  -- apply recursively
                  else do let l = fromJust ml
                          tobnd_add l CEmpty l
                          push
                          two_watch_all
                          r <- cdcl
                          -- Either in restart or satisfied, anyway end the
                          -- process and return
                          if r /= Just False then do collapse
                                                     return r
                          else do pop
                                  b <- stop_back l
                                  -- If the backtracking process makes the
                                  -- learnt clause true, test the other value
                                  -- for the choosen variable. If not,
                                  -- backtrack further.
                                  if b then do clear_error >> clear_new
                                               tobnd_add (neg l) CEmpty l
                                               two_watch_all
                                               -- Take care to remove new
                                               -- clause from backtracking
                                               -- constraint if necessary
                                               r <- cdcl
                                               b2 <- stop_back l
                                               if r == Just False && b2
                                               then clear_new
                                               else return ()
                                               return r
                                       else return $ Just False

solver :: Chooser a => MdSt a (Maybe (Array Literal Bool))
solver = do init_chooser
            r <- tryuntil test (\i -> setre i >> clear >> cdcl) restarts
            -- Restart until either SAT or UNSAT in returned
            let b = fromJust r
            if b then do v <- MdSt $ \s -> (s,head $ vars_st s)
                         return $ Just $ mapArray fj v
                 else return Nothing
 where restarts = [2 ^ i | i <- [8..]]
       luby = [ let k = 1 + (floor $ log2 i) in if i == 2^k - 1 then 2^(k-1)
                                                else luby !! (i - 2^(k-1))
              | i <- [1..]]
       u = 32 -- to implement the luby32 strategy
       log2 :: Int -> Float
       log2 x = log (fromIntegral x) / log 2
       test x = return $ x /= Nothing
       setre i = MdSt $ \s -> (s {restart_st = i}, ())
       clear = clear_vars >> clear_error >> clear_new
       fj Nothing  = True
       fj (Just v) = v

-- }}}

solve :: Chooser a => a -> (Int,CNF) -> Maybe (Array Literal Bool)
solve c (n,sat) = runMdSt n sat c $ solver

