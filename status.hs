-- vim:set foldmethod=marker:

module SAT.Status where
import qualified Data.Vector as V
import SAT.Structures

type MBool = Maybe Bool
data Status a = Status
    { vars_st    :: [V.Vector MBool]
    , bound_st   :: V.Vector Clause -- Remember which clause bound the variable
    , level_st   :: V.Vector Literal -- Remember the level (literal decision)
                                     -- which caused the variable to be bound
    , sat_st     :: CNF
    , error_st   :: Clause -- CEmpty if no contradiction, else the clause
                           -- source of the contradiction
    , new_st     :: Clause -- Last learnt clause, for bactracking
    , restart_st :: Int -- Number of tries before restart
    , chooser_st :: a -- Data for the function choosing the new variable to set
    , tobnd_st   :: [(Literal,Clause,Literal)] -- Literals that must be bound
    }
instance Show (Status a) where
    show s = "Status :"
           ++ "\n\tvars = "    ++ show (vars_st s)
           ++ "\n\tbounds = "  ++ show (bound_st s)
           ++ "\n\tlevels = "  ++ show (level_st s)
           ++ "\n\tsat  = "    ++ show (sat_st s)
           ++ "\n\terror = "   ++ show (error_st s)
           ++ "\n\tnew = "     ++ show (new_st s)
           ++ "\n\trestart = " ++ show (restart_st s)
           ++ "\n\ttobnd = "   ++ show (tobnd_st s)
           ++ "\n\n"

mkStatus :: Int -> CNF -> a -> Status a
mkStatus n sat c = Status
    { vars_st    = V.fromList [Nothing | i <- [0..n]] : []
    , bound_st   = V.fromList [CEmpty  | i <- [0..n]]
    , level_st   = V.fromList [L 0     | i <- [0..n]]
    , sat_st     = sat
    , error_st   = CEmpty
    , new_st     = CEmpty
    , restart_st = 0
    , chooser_st = c
    , tobnd_st   = []
    }

class Chooser a where
    ch_choose  :: Status a -> (a, Maybe Literal)
    ch_conflit :: Status a -> Clause -> a
    ch_init    :: Status a -> a

