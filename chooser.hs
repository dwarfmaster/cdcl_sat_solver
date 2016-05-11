-- vim:set foldmethod=marker:

module SAT.Choosers where
import SAT.Status
import qualified Data.Array as A

-- {{{ () chooser
-- Choose the first free variable an set it to True
instance Chooser () where
    ch_choose  s   = if f == [] then ((),Nothing) else ((),Just $ head f)
     where f = [i | (i,e) <- A.assocs v, e == Nothing]
           v = head $ vars_st s
    ch_conflit _ _ = ()
    ch_init    _   = ()

-- }}}
