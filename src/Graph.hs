module Graph where
  -- TODO implement graph with search adjacent by vertex index and attr name
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  type VertexId = Int
  type AttributeName = String
  type VertexData = String
  type EdgeId = Int
  type SpecialLabel = String
  data EdgeColor = Black | Orange | Blue deriving (Show)
  type EdgeEnds = (VertexId, VertexId)

  data Edge =
    Edge {
    ends :: EdgeEnds,
    attribute :: AttributeName,
    specialLabel :: SpecialLabel,
    color :: EdgeColor
  } deriving (Show)

  data Graph =
    Graph {
    -- adjacent vertex by attribute
    vertexData :: Map.Map VertexId VertexData,
    edgeData :: Map.Map EdgeId Edge,
    vertexCount :: Int,
    edgeCount :: Int,
    queried :: Set.Set String
  } deriving (Show)

  defaultEdge =
    Edge {
    ends = (-1, -1),
    attribute = "",
    specialLabel = "",
    color = Black
  }

  emptyGraph :: Graph
  emptyGraph =
    Graph {
    vertexData = Map.empty,
    edgeData = Map.empty,
    vertexCount = 0,
    edgeCount = 0,
    queried = Set.empty
  }

  -- no vertice id arg to guarrantee uniqueness of vertices
  addVertex :: VertexId -> Graph -> Graph
  addVertex v1 g =
    g {
    vertexData = Map.insert v1 "" (vertexData g)
    ,
    vertexCount = (vertexCount g) + 1
  }

  getEdge :: EdgeId -> Graph -> Edge
  getEdge edgeId g = Map.findWithDefault defaultEdge edgeId (edgeData g)

  _rho_ :: String
  _rho_ = "𝜌"

  insertEdge :: VertexId -> VertexId -> AttributeName -> EdgeColor -> Graph -> Graph
  insertEdge v1 v2 attr color g =
    g {
    edgeData =
      Map.insert
        (edgeCount g)
        defaultEdge {ends = (v1, v2), attribute = attr, color = color}
        (edgeData g)
    ,
    edgeCount = (edgeCount g) + 1
  }

  deleteEdge :: VertexId -> Graph -> Graph
  deleteEdge v1 g =
    g {
    edgeData = Map.delete v1 (edgeData g)
  }

  command :: Show a => [Char] -> a -> [Char]
  command cmdName attrs = cmdName ++ show attrs

  isQueried cmd g = Set.member cmd (queried g)

  bind :: VertexId -> VertexId -> AttributeName -> Graph -> Graph
  bind v1 v2 attr g
      | isQueried (command "bind" (v1,v2,attr)) g = g
      | attr == _rho_ =
          insertEdge v1 v2 attr Orange g
      | otherwise =
          insertEdge v1 v2 attr Black
          $ insertEdge v2 v1 _rho_ Orange g

  atom :: VertexId -> VertexData -> Graph -> Graph
  atom v1 m1 g =
    g {
    vertexData = Map.insert v1 m1 (vertexData g)
  }

  dot :: EdgeId -> AttributeName -> VertexId -> EdgeId -> Graph -> Graph
  dot e1 m v3 e2 g
    | isQueried (command "dot" (e1, m, v3, e2)) g = g
    | otherwise =
          deleteEdge e1
          $ insertEdge v3 v2 "t" Black
          $ insertEdge v1 v3 attr Black
          $ atom v3 ("R(ksi.t," ++ m ++ ",s)") g
    where
      edge1 = getEdge e1 g
      (v1, v2) = ends edge1
      attr = attribute edge1

  copy :: EdgeId -> VertexId -> EdgeId -> Graph -> Graph
  copy e1 v3 e2 g
      | isQueried (command "copy" (e1, v3, e2)) g = g
      | otherwise =
            deleteEdge e1
            $ insertEdge v3 v2 "" Blue
            $ insertEdge v1 v3 attr Black g
      where
        edge1 = getEdge e1 g
        (v1, v2) = ends edge1
        attr = attribute edge1


  -- TODO ref

  -- type Locator = String


  -- ref :: EdgeId -> VertexId -> Locator -> AttributeName -> Graph -> Graph
  -- ref e1 v1 l a =
  --     | isQueried (command "ref" (e1, v1, l, a)) g = g
  --     | otherwise =
