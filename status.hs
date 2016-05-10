-- vim:set foldmethod=marker:

module SAT.Status where
import Data.Array (Array,range)
import qualified Data.Array as A
import SAT.Structures

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
instance Show (Status a) where
    show s = "Status :"
           ++ "\n\tvars = "    ++ show (vars_st s)
           ++ "\n\tsat  = "    ++ show (sat_st s)
           ++ "\n\terror = "   ++ show (error_st s)
           ++ "\n\tnew = "     ++ show (new_st s)
           ++ "\n\trestart = " ++ show (restart_st s)
           ++ "\n\ttobnd = "   ++ show (tobnd_st s)
           ++ "\n\n"

mkStatus :: Int -> CNF -> a -> Chooser a -> Status a
mkStatus n sat d c = Status
    { vars_st    = A.array (L 1,L n) [(L i,Nothing) | i <- range (1,n)] : []
    , sat_st     = sat
    , error_st   = False
    , new_st     = Nothing
    , restart_st = 0
    , chdata_st  = d
    , chooser_st = c
    , tobnd_st   = []
    }

