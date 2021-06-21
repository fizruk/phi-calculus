module Graph where
  -- TODO implement graph with search adjacent by vertex index and attr name
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  type VertexId = Int
  type VertexName = String
  type AttributeName = String
  type VertexData = String

  data Graph =
    Graph {
    -- adjacent vertex by attribute
    attributes :: Map.Map (VertexId, AttributeName) ToVertex,
    vertexIdByName :: Map.Map String VertexId,
    vertexData :: Map.Map VertexId VertexData,
    vertexCount :: Int
  } deriving (Show)

  data ToVertex =
    ToVertex {
    vertexId :: VertexId,
    edgeColor :: EdgeColor
  } deriving (Show)

  data EdgeColor = Black | Orange | Blue deriving (Show)

  defaultToVertex =
    ToVertex {
    vertexId = -1,
    edgeColor = Black
  }

  emptyGraph :: Graph
  emptyGraph =
    Graph {
    attributes = Map.fromList  ([((-1,""), defaultToVertex)]),
    vertexIdByName = Map.empty,
    vertexData = Map.empty,
    vertexCount = 0
  }

  -- vertex as string
  addVertex :: VertexName -> Graph -> Graph
  addVertex vertexName g =
    g {
    vertexIdByName = Map.insert vertexName (vertexCount g) (vertexIdByName g),
    vertexData = Map.insert (vertexCount g) "" (vertexData g),
    vertexCount = (vertexCount g) + 1
  }


  bind :: VertexName -> VertexName -> AttributeName -> Graph -> Graph
  bind v1 v2 attr g =
    g {
    attributes =
      let
        id1 =
          case Map.lookup v1 (vertexIdByName g) of
            Just s -> s
            otherwise -> -1
        id2 =
          case Map.lookup v2 (vertexIdByName g) of
            Just s -> s
            otherwise -> -1
        insertFrom id1 =
          Map.insert (id1, attr) ToVertex {vertexId = id2, edgeColor = Black}
        insertTo id1 =
          Map.insert (id2, "𝜌") ToVertex {vertexId = id1, edgeColor = Orange}
        insertRhoFrom id1 =
          Map.insert (id1, "𝜌") ToVertex {vertexId = id2, edgeColor = Orange}
      in
        case attr of
          "𝜌" -> insertRhoFrom id1 (attributes g)
          otherwise -> insertFrom id1 $ insertTo id1 (attributes g)
  }
