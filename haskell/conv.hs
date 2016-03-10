
import System.IO

data Expr = Var Int
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Const Bool
 deriving (Show,Eq)

type Literal = Either Int Bool
type Clause = [Literal]
type CNF = [Clause]

cross :: CNF -> CNF -> CNF
cross l1 l2 = [a++b | a <- l1, b <- l2]

convert :: Expr -> CNF
convert (Var n) = [[Left n]]
convert (And e1 e2) = let (c1,c2) = (convert e1, convert e2) in c1 ++ c2
convert (Or e1 e2)  = let (c1,c2) = (convert e1, convert e2) in cross c1 c2
convert (Not e)     = case e of
    Var n     -> [[Left (-n)]]
    And e1 e2 -> convert (Or (Not e1) (Not e2))
    Or  e1 e2 -> convert (And (Not e1) (Not e2))
    Const b   -> [[Right $ not b]]
    Not e     -> convert e
convert (Const b)   = [[Right b]]

litToString :: Literal -> String
litToString (Left  a) = show a
litToString (Right a) = show a

clauseToString :: Clause -> String
clauseToString []    = ""
clauseToString (h:t) = litToString h ++ " " ++ clauseToString t

cnfToString :: CNF -> String
cnfToString []    = ""
cnfToString (h:t) = (clauseToString h) ++ "\n" ++ cnfToString t

writeCNF :: CNF -> String -> IO ()
writeCNF cnf s = do
    file <- openFile s WriteMode
    hPutStrLn file str
    hClose file
 where str = cnfToString cnf


