module Graph where

type DataType = String

data EdgeType = Orange | Black | Blue deriving (Show, Eq)

data Edge = Edge {start :: DataType, end :: DataType, edgeData :: DataType, edgeType :: EdgeType}
  deriving (Show, Eq)

data Vertex = Vertex {vertexData :: DataType, adjacentEdges :: [Edge], lambdaTerm :: String}
  deriving (Show, Eq)

data DataGraph = DataGraph {verteces :: [Vertex], edges :: [Edge]}
   deriving (Show, Eq)

add :: DataGraph -> Vertex -> DataGraph
add g v = DataGraph {verteces = v : verteces g, edges = edges g}

bindUpdateVerteces :: Vertex -> Edge -> DataType -> Vertex
bindUpdateVerteces v e d =
  if vertexData v == d
    then
      Vertex
        { vertexData = vertexData v,
          lambdaTerm = lambdaTerm v,
          adjacentEdges = e : adjacentEdges v
        }
    else v

--  startVertex = head (filter (\x -> vertexData x == start) (verteces graph))
--         endVertex = head (filter (\x -> vertexData x == end) (verteces graph))

bind :: DataGraph -> DataType -> DataType -> Edge -> DataGraph
bind graph startName endName edge =
  let reverseEdge = Edge {start = end edge, end = start edge, edgeData = "", edgeType = Orange}
      newVertecesList =
        map
          ( (\x -> bindUpdateVerteces x reverseEdge endName)
              . (\x -> bindUpdateVerteces x edge startName)
          )
          (verteces graph)
   in DataGraph
        { verteces = newVertecesList,
          edges = reverseEdge : edge : edges graph
        }

updateCopy :: Vertex -> Edge -> Edge -> Vertex
updateCopy vertex edgeDel edgeAdd =
  if vertexData vertex == start edgeDel
    then
      Vertex
        { vertexData = vertexData vertex,
          lambdaTerm = lambdaTerm vertex,
          adjacentEdges = edgeAdd : filter (/= edgeDel) (adjacentEdges vertex)
        }
    else vertex

copy :: DataGraph -> Edge -> Vertex -> Edge -> DataGraph
copy graph firstEdge vertex secondEdge =
  let newEdge =
        Edge
          { start = vertexData vertex,
            end = end firstEdge,
            edgeType = Blue,
            edgeData = ""
          }
      newEdgesList = newEdge : secondEdge : filter (/= firstEdge) (edges graph)
      updatedVertex =
        Vertex
          { vertexData = vertexData vertex,
            lambdaTerm = lambdaTerm vertex,
            adjacentEdges = [newEdge]
          }
   in DataGraph {verteces = map (\x -> updateCopy x firstEdge secondEdge) (verteces graph), edges = newEdgesList}

-- atom :: DataGraph -> Vertex -> String -> DataGraph
-- atom graph v lambda =
--     let updatedVertecesList = map (\x -> if vertexData x == vertexData v then
--                                         Vertex{vertexData = vertexData x, adjacentEdges = adjacentEdges x, lambdaTerm = lambda}
--                                         else x) (verteces graph) in
--     DataGraph {verteces = updatedVertecesList, edges = edges graph}

atom :: Vertex -> String -> Vertex
atom v l = Vertex {vertexData = vertexData v, adjacentEdges = adjacentEdges v, lambdaTerm = l}

udpateVertexDot :: Vertex -> DataType -> Vertex
udpateVertexDot v s =
  Vertex
    { vertexData = vertexData v,
      adjacentEdges = filter (\x -> edgeData x /= s) (adjacentEdges v),
      lambdaTerm = lambdaTerm v
    }

dot :: DataGraph -> Edge -> String -> Vertex -> Edge -> DataGraph
dot graph e1 m v3 e2 =
  let newEdge = Edge {start = vertexData v3, end = end e1, edgeData = "toDo", edgeType = Black}
      updatedListOfEdges = filter (\x -> edgeData x /= edgeData e1) (edges graph)
      updatedListOfVerteces =
        map (\x -> if vertexData x == start e1 then udpateVertexDot x (edgeData e1) else x) (verteces graph)
      updatedV3 = atom v3 m
      newV3 =
        Vertex
          { vertexData = vertexData updatedV3,
            adjacentEdges = newEdge : adjacentEdges updatedV3,
            lambdaTerm = lambdaTerm updatedV3
          }
   in DataGraph {verteces = newV3 : updatedListOfVerteces, edges = newEdge : e2 : updatedListOfEdges}

printVertex :: Vertex -> IO ()
printVertex v = do
  putStr "vertex name: "
  print (vertexData v)
  putStr "list of adjacent edges: "
  print e
  where
    e = map edgeData (adjacentEdges v)

printGraph g = mapM_ printVertex (verteces g)