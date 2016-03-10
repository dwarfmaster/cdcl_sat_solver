
import Data.Set (Set)
import qualified Data.Set as Set

type Literal = Int
data Clause = C0 Bool
            | C1 Literal
            | C2 Literal Literal
            | C3 Literal Literal Literal
type CNF = [Clause] -- Empty means true

-- The first set is the one assumed to be true
-- The second set is the one assumed to be false
type Assum = (Set Literal, Set Literal)

-- Add if not nothing
(*-) :: Maybe a -> [a] -> [a]
(*-) Nothing  l = l
(*-) (Just v) l = v : l

boundValue :: Literal -> Assum -> Maybe Bool
boundValue l (tr,fl) = if l < 0 then if      member (-l) tr then Just False
                                     else if member (-l) fl then Just True
                                     else                        Nothing
                       else          if      member l tr then Just True
                                     else if member l fl then Just False
                                     else                     Nothing

reduceClause :: Clause -> Assum -> Assum
reduceClause (C0 b) _ = b
reduceClause (C1 l) _ = if l < 0 then (empty, singleton (-l))
                                 else (singleton l, empty)
reduceClause (C2 l1 l2) as = -- TODO
 where v1 = boundValue l1 as
       v2 = boundValue l2 as

