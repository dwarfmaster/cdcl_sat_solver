-- vim:set foldmethod=marker:

import Data.Array (Array,(!),(//)
                   ,Ix,range,index,inRange)
import qualified Data.Array as A
import Data.Maybe

-- {{{ Data structures
data Literal = L Int
instance Eq Literal where
    (==) (L a) (L b) = abs a == abs b
instance Ord Literal where
    (<=) (L a) (L b) = abs a <= abs b
instance Ix Literal where
    range   (L a,L b)       = map L $ range (abs a,abs b)
    index   (L a,L b) (L c) = index (abs a,abs b) $ abs c
    inRange (L a,L b) (L c) = inRange (abs a,abs b) $ abs c
instance Show Literal where
    show (L a) = "L " ++ show a

type Clause = [Literal]
type CNF = [Clause]

type MBool = Maybe Bool
data Status a = Status
    { vars_st    :: [Array Literal MBool]
    , sat_st     :: CNF
    , error_st   :: Bool -- True if vars are a contradiction
    , new_st     :: Maybe Clause -- Last learnt clause, for bactracking
    , restart_st :: Int -- Number of tries before restart
    , chdata_st  :: a -- Data for the function choosing the new variable to set
    , chooser_st :: Chooser a
    , tobnd_st   :: [Literal] -- Literals that must be bound
    }
type Chooser a = Status a -> (Status a,Maybe Literal)

mkStatus :: Int -> Int -> CNF -> a -> Chooser a -> Status a
mkStatus n re sat d c = Status
    { vars_st    = A.array (L 1,L n) [(L i,Nothing) | i <- range (1,n)] : []
    , sat_st     = sat
    , error_st   = False
    , new_st     = Nothing
    , restart_st = re
    , chdata_st  = d
    , chooser_st = c
    , tobnd_st   = []
    }

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

runMdSt :: Int -> Int -> CNF -> a -> Chooser a -> MdSt a b -> b
runMdSt n re sat d c (MdSt m) = snd $ m $ mkStatus n re sat d c

-- }}}

-- {{{ Tools
neg :: Literal -> Literal
neg (L l) = L $ -l

sgn :: Literal -> Bool
sgn (L l) = if l < 0 then False else True

choose :: MdSt a (Maybe Literal)
choose = MdSt $ \s -> (chooser_st s) s

mult :: Literal -> MBool -> MBool
mult _     Nothing  = Nothing
mult (L l) (Just b) = if l < 0 then Just $ not b else Just b

status :: Literal -> MdSt a MBool
status l = MdSt $ \s -> (s,mult l $ (head $ vars_st s) ! l)

bind :: Literal -> MdSt a ()
bind l = MdSt $ \s -> let h:t = vars_st s in
                      (s {vars_st = h // [(l,Just $ sgn l)] : t}, ())

push :: MdSt a ()
push = MdSt $ \s -> let v = vars_st s in
                    (s {vars_st = head v : v}, ())

pop :: MdSt a ()
pop = MdSt $ \s -> let v = vars_st s in
                   (s {vars_st = tail v}, ())

sat_get :: MdSt a CNF
sat_get = MdSt $ \s -> (s,sat_st s)

new_clause :: MdSt a (Maybe Clause)
new_clause = MdSt $ \s -> (s,new_st s)

-- Test if restart is necessary
should_restart :: MdSt a Bool
should_restart = MdSt $ \s -> (s, restart_st s == 0)

-- Decrement the restart counter
dec_restart :: MdSt a ()
dec_restart = MdSt $ \s -> let n = restart_st s in
                           if n == 0 then (s, ())
                                     else (s {restart_st = n - 1}, ())

is_error :: MdSt a Bool
is_error = MdSt $ \s -> (s,error_st s)

launch_error :: MdSt a ()
launch_error = MdSt $ \s -> (s {error_st = True}, ())

clear_error :: MdSt a ()
clear_error = MdSt $ \s -> (s {error_st = False}, ())

-- Return and remove a literal to bound
tobnd_get :: MdSt a (Maybe Literal)
tobnd_get = MdSt $ \s -> case tobnd_st s of
                         []  -> (s, Nothing)
                         h:t -> (s {tobnd_st = t}, Just h)

tobnd_peek :: MdSt a (Maybe Literal)
tobnd_peek = MdSt $ \s -> (s, case tobnd_st s of
                              []  -> Nothing
                              h:_ -> Just h)

tobnd_add :: Literal -> MdSt a ()
tobnd_add h = MdSt $ \s -> (s {tobnd_st = h : tobnd_st s}, ())

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
foreach :: (a -> MdSt b ()) -> [a] -> MdSt b ()
foreach f []    = return ()
foreach f (h:t) = do f h
                     foreach f t

formap :: (a -> MdSt b c) -> [a] -> MdSt b [c]
formap f []    = return []
formap f (h:t) = do c <- f h
                    l <- formap f t
                    return $ c : l

while :: MdSt a Bool -> MdSt a () -> MdSt a ()
while f g = do b <- f
               if b then do { g; while f g } else return ()

-- }}}

-- {{{ CDCL
-- Apply two-watch simplification to a clause
two_watch :: Clause -> MdSt a Clause
two_watch []     = return []
two_watch (h:[]) = -- Shouldn't happen, as a preprocessor should have
                   -- simplified singles clauses
    do b <- status h
       if b == Just False then launch_error
       else if b == Nothing then tobnd_add h
       else return ()
       return [h]
two_watch l@(h1:h2:t) =
    do b <- r h1
       nc <- if b then do n <- raise_on r $ h2 : t
                          return $ h1 : n
                  else raise_on r $ l
       let n1:n2:_ = nc
       s1 <- status n1
       s2 <- status n2
       if s1 == Just False then launch_error
       else if s2 == Just False && s1 == Nothing then tobnd_add n1
       else return ()
       return nc
 where r l = do b <- status l
                return $ b /= (Just False)

-- Apply two-watch simplification to all clauses
two_watch_all :: MdSt a ()
two_watch_all = while test $ do
    tb <- tobnd_get
    bind $ fromJust tb
    cnf <- sat_get
    formap two_watch cnf
    return ()
 where test = do e  <- is_error
                 tb <- tobnd_peek
                 return $ not e && tb /= Nothing

-- The cdcl algorithm, ending on restarts
-- Returns Nothing if it ended due to a restart, Just True is the problem is
-- SAT and Just False if UNSAT
cdcl :: MdSt a (Maybe Bool)
cdcl = do ml <- choose
          if ml == Nothing then do e <- is_error
                                   if not e then return $ Just True
                                   else do dec_restart
                                           b <- should_restart
                                           return $ if b then Nothing
                                                         else Just False
          else do let l = fromJust ml
                  bind l
                  push
                  two_watch_all
                  r <- cdcl
                  if r /= Just False then return r
                  else do pop
                          bind $ neg l
                          two_watch_all
                          cdcl

-- }}}

