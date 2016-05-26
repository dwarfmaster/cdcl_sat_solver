-- vim:set foldmethod=marker:

module SAT.Choosers where
import SAT.Structures
import SAT.Status
import Data.Vector (Vector,(!),(//))
import qualified Data.Vector as V

-- {{{ () chooser
-- Choose the first free variable an set it to True
instance Chooser () where
    ch_choose  s   = if f == [] then ((),Nothing) else ((),Just $ head f)
     where f = [L i | i <- [1..n], v!i == Nothing]
           n = V.length v - 1
           v = head $ vars_st s
    ch_conflit _ _ = ()
    ch_init    _   = ()

-- }}}

-- {{{ VSIDS chooser
data VSIDS = VSIDS (Vector Float) -- the activities
                   Float          -- the decay
                   Float          -- the bump
                   Int            -- how often to bump
                   Int            -- conflicts since last bump

instance Chooser VSIDS where
    ch_choose s = if f == [] then (vs,Nothing)
                  else (vs, Just $ L $ snd $ foldl1 get f)
     where f = [(a ! i,i) | i <- [1..n], v ! i == Nothing]
           n = V.length a - 1
           v = head $ vars_st s
           vs@(VSIDS a _ _ _ _) = chooser_st s
           get a@(v,_) b@(w,_) = if v < w then b else a

    -- Init all to 0
    ch_init s = VSIDS a d b i c
     where VSIDS _ d b i c = chooser_st s
           n = (V.length $ head $ vars_st s) - 1
           a = V.fromList [0 | i <- [0..n]]

    ch_conflit s (OR cl) = VSIDS nna d b i nc
     where VSIDS a d b i c = chooser_st s
           n   = V.length a - 1
           nc  = if c + 1 == i then 0 else c + 1
           na_ = a // [(i,a!i + b) | i <- [1..n], elem (L i) cl]
           na  = if nc == 0 then na_ else a
           nna = a // [(i,a!i * d) | i <- [1..n]]
    ch_conflit s _ = chooser_st s

mkVSIDS :: Int   -- how often to bump
        -> Float -- the bump
        -> Float -- the decay
        -> VSIDS
mkVSIDS i b d = VSIDS (V.fromList [0]) d b i 0
defVSIDS = mkVSIDS 1 1.0 0.5
-- }}}


