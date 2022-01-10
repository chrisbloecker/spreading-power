module Model
    where
--------------------------------------------------------------------------------
import Data.Map.Strict (Map)
import Data.Set        (Set)
--------------------------------------------------------------------------------

type Node = String

type Edge = (Node, Node)

-- we represent a graph by listing nodes' neighbours
newtype Graph = Graph { neighbours :: Map Node (Set Node) }
    deriving (Show)