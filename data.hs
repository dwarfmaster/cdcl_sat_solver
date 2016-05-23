-- vim:set foldmethod=marker:

module SAT.Structures (Literal(L),Clause(OR,XOR,CEmpty),CNF) where
import Data.Array (Ix,range,index,inRange)

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

data Clause = OR [Literal] | XOR [Literal] | CEmpty
    deriving (Eq)
instance Show Clause where
    show (OR l)  = "OR  " ++ show l
    show (XOR l) = "XOR " ++ show l
    show CEmpty  = "[empty clause]"
type CNF = [Clause]

