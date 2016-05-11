-- vim:set foldmethod=marker:

module SAT.Choosers where
import SAT.Structures
import SAT.Status
import Data.Array (Array,(!),(//))
import qualified Data.Array as A

-- {{{ Helpers
elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy c x l = foldl test False l
 where test b a = b || c x a

cmpL :: Literal -> Literal -> Bool
cmpL (L i) (L j) = i == j
-- }}}

-- {{{ () chooser
-- Choose the first free variable an set it to True
instance Chooser () where
    ch_choose  s   = if f == [] then ((),Nothing) else ((),Just $ head f)
     where f = [i | (i,e) <- A.assocs v, e == Nothing]
           v = head $ vars_st s
    ch_conflit _ _ = ()
    ch_init    _   = ()

-- }}}

-- {{{ VSIDS chooser
data VSIDS = VSIDS (Array Int Float) -- the activities
                   Float             -- the decay
                   Float             -- the bump
                   Int               -- how often to bump
                   Int               -- conflicts since last bump

instance Chooser VSIDS where
    ch_choose s = if f == [] then (vs,Nothing)
                  else (vs, Just $ L $ snd $ foldl1 get f)
     where f = [(a ! i,i) | i <- A.range (A.bounds a)
                          , v ! (L i) == Nothing, i /= 0]
           v = head $ vars_st s
           vs@(VSIDS a _ _ _ _) = chooser_st s
           get a@(v,_) b@(w,_) = if v < w then b else a
    -- Init by counting how many clauses contain each variable
    ch_init s = VSIDS na d b i c
     where VSIDS a d b i c = chooser_st s
           sat   = sat_st s
           bnd   = A.bounds a
           na    = A.array bnd [(i,fromIntegral $ length (cls i))
                               | i <- A.range bnd]
           cls i = [cl | cl <- sat, elemBy cmpL (L i) cl]
    ch_conflit s cl = VSIDS nna d b i nc
     where VSIDS a d b i c = chooser_st s
           nc  = if c + 1 == i then 0 else c + 1
           na_ = a // [(i,a!i + b) | i <- A.range (A.bounds a)
                                   , elemBy cmpL (L i) cl]
           na  = if nc == 0 then na_ else a
           nna = a // [(i,a!i * d) | i <- A.range (A.bounds a)]

mkVSIDS :: Int   -- how often to bump
        -> Float -- the bump
        -> Float -- the decay
        -> Int   -- the number of variables
mkVSIDS i b d n = VSIDS (A.array (-n,n) [(i,0) | i <- A.range (-n,n)])
                        d b i 0
defVSIDS = mkVSIDS 1 1.0 0.5
-- }}}


