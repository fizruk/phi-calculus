module Graph where

-- TODO implement graph with search adjacent by vertex index and attr name

import Commands (Command (..), VertexId)
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import PhiGrammar (AttributeName, Lambda, Locator, Term (..))

data EdgeType
  = AttributeEdge AttributeName
  | CopyEdge
  deriving (Show, Eq, Ord)

data Graph = Graph
  { vertexLambda :: Map.Map VertexId Lambda,
    edge :: Map.Map (VertexId, EdgeType) VertexId
  }
  deriving (Show, Eq, Ord)

executeCommand :: Command -> (Graph -> Graph)
executeCommand cmd graph =
  case cmd of
    ADD v -> add v
    BIND v1 v2 a -> bind v1 v2 a
    COPY v1 v2 a -> copy e1 v e2
    ATOM v1 v2 a m -> atom v1 v2 a m

-- | execute a sequence of GMIs
executeCommands :: [Command] -> Graph -> Graph
executeCommands [] = id
executeCommands (cmd : cmds) = executeCommands cmds . executeCommand cmd

-- | initial graph without vertices
emptyGraph :: Graph
emptyGraph =
  Graph
    { vertexLambda = Map.empty,
      edge = Map.empty
    }

-- | add a vertex with the next unused number to the graph
add :: VertexId -> Graph -> Graph
add v g = g

bind :: VertexId -> VertexId -> AttributeName -> Graph -> Graph
bind v1 v2 a g =
  g
    { edge = Map.insert (v1, AttributeEdge a) v2 edge
    }

copy :: VertexId -> VertexId -> Graph -> Graph
copy v1 v2 g =
  g
    { edge = Map.insert (v1, CopyEdge) v2 edge
    }

atom :: VertexId -> Lambda -> Graph -> Graph
atom v1 m1 g = g {vertexLambda = Map.insert v1 m1 vertexLambda}

defaultVertexId :: VertexId
defaultVertexId = -1

-- | get adjacent vertex by attribute
goToAdjacent :: VertexId -> AttributeName -> Graph -> VertexId
goToAdjacent v a g =
  case Map.lookup (v, a) (edge g) of
    Just x -> x
    Nothing ->
      error
        ( printf
            "Edge from %d with attrubute %s doesn't exist"
            v
            (linePrint a)
        )

-- executeSampleCommands :: Graph -> Graph
-- executeSampleCommands g =
--   g
--     & executeCommands
--       [ ADD, -- vertex 0
--         ADD, -- vertex 1
--         ATOM 1 "M1",
--         BIND 0 1 (Attr "memory"),
--         ADD, -- vertex 2
--         BIND 0 2 (Attr "book2"),
--         ADD, -- vertex 3
--         BIND 2 3 (Attr "isbn"),
--         ADD, -- vertex 4
--         BIND 2 4 (Attr "title"),
--         REF 2 [UpperPhi, Attr ".memory"] (Attr "price"),
--         DOT 3 (Attr "m") (edgeCount g)
--       ]

{-
>>> executeSampleCommands emptyGraph
Graph {vertexLambda = fromList [(1,"M1"),(5,"R(_xi_.t,\"m\",s)")], edgeData = fromList [(0,Edge {ends = (1,0), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(1,Edge {ends = (0,1), attribute = "memory", specialLabel = [], edgeType = AttributeEdge}),(2,Edge {ends = (2,0), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(4,Edge {ends = (3,2), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(5,Edge {ends = (2,3), attribute = "isbn", specialLabel = [], edgeType = AttributeEdge}),(6,Edge {ends = (4,2), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(7,Edge {ends = (2,4), attribute = "title", specialLabel = [], edgeType = AttributeEdge}),(8,Edge {ends = (2,-1), attribute = "price", specialLabel = [_Phi_,".memory"], edgeType = ParentEdge}),(9,Edge {ends = (5,2), attribute = t, specialLabel = [], edgeType = ParentEdge}),(10,Edge {ends = (0,5), attribute = "book2", specialLabel = [], edgeType = AttributeEdge})], edge = fromList [((0,"memory"),1),((1,_rho_),0),((2,"isbn"),3),((2,"price"),-1),((2,"title"),4),((2,_rho_),0),((3,_rho_),2),((4,_rho_),2),((5,t),2)], vertexCount = 6, edgeCount = 11, queried = fromList [BIND 0 1 "memory",BIND 0 2 "book2",BIND 2 3 "isbn",BIND 2 4 "title",DOT 3 "m" 0,ATOM 1 "M1",REF 2 [_Phi_,".memory"] "price"]}
-}

-- calculate lambda and return locator of vertex with lambda's result
evaluateLambda :: Lambda -> Vector -> Locator
evaluateLambda d v = stop

type Vector = [Locator]

discover :: Locator -> Attribute -> Vector -> Graph -> Locator
discover l a s g
  | hasAAttribute = appendTo a l
  | hasLowerPhiAttribute =
    let tau = LowerPhi `appendTo` l
     in discover tau a (tau : s) g
  | hasCopyAttribute =
    let tau = Copy `appendTo` l
     in discover tau a s g
  | hasLambda =
    discover (evaluateLambda m (v `appendTo` s)) a s g
  | otherwise = stop
  where
    ifHasDot =
      \x ->
        case x of
          _ : _ -> discover (init x) (last x) s g
          _ -> x
    ifXi =
      \x ->
        case x of
          [Xi] -> head s
          _ -> x
    ifRho =
      \x ->
        case x of
          [Rho] -> s !! 1
          _ -> x
    v = l & ifHasDot & ifXi & ifRho
    w = findByLocator 0 v g
    a `appendTo` l = reverse (a : reverse l)
    m = Map.findWithDefault "" w (vertexLambda g)
    hasAAttribute = goToAdjacent w a g /= defaultVertexId
    hasLowerPhiAttribute = goToAdjacent w LowerPhi g /= defaultVertexId
    hasCopyAttribute = goToAdjacent w Copy g /= defaultVertexId
    hasLambda = m /= ""
