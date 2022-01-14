{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Model
    where
--------------------------------------------------------------------------------
import Data.Map.Strict (Map)
import Data.Set        (Set)
--------------------------------------------------------------------------------
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
--------------------------------------------------------------------------------

type Node = String

type Edge = (Node, Node)

-- we represent a graph by listing nodes' neighbours
newtype Graph = Graph { neighbours :: Map Node (Set Node) }
    deriving (Show)

nodes :: Graph -> [Node]
nodes Graph{..} = S.toList $ M.keysSet neighbours `S.union` S.unions (M.elems neighbours)