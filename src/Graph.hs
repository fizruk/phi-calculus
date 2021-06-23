module Graph where
  -- TODO implement graph with search adjacent by vertex index and attr name
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  type VertexId = Int
  type AttributeName = String
  type VertexData = String
  type EdgeId = Int
  type SpecialLabel = String
  data EdgeColor = Black | Orange | Blue | Green deriving (Show)
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
    vertexData :: Map.Map VertexId VertexData,
    edgeData :: Map.Map EdgeId Edge,
    attributeVertex :: Map.Map (VertexId, AttributeName) VertexId,
    vertexCount :: Int,
    edgeCount :: Int,
    queried :: Set.Set String
  } deriving (Show)

  defaultEdge :: Edge
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
    attributeVertex = Map.empty,
    vertexCount = 0,
    edgeCount = 0,
    queried = Set.empty
  }

  add :: Graph -> Graph
  add g =
    g {
    vertexData = Map.insert (vertexCount g) "" (vertexData g)
    ,
    vertexCount = (vertexCount g) + 1
  }

  getEdge :: EdgeId -> Graph -> Edge
  getEdge edgeId g = Map.findWithDefault defaultEdge edgeId (edgeData g)

  _rho_ :: String
  _rho_ = "_rho_"

  addEdge :: Edge -> Graph -> Graph
  addEdge e g =
    g {
    edgeData = Map.insert (edgeCount g) e (edgeData g)
    ,
    edgeCount = (edgeCount g) + 1
    ,
    attributeVertex = Map.insert (v1, attr) v2 (attributeVertex g)
  }
    where
      (v1, v2) = ends e
      attr = attribute e

  deleteEdge :: EdgeId -> Graph -> Graph
  deleteEdge e g =
    g {
    edgeData = Map.delete e (edgeData g)
    ,
    attributeVertex =
      Map.delete (v1, attr) $ attributeVertex g
  }
    where
      edge = getEdge e g
      (v1, _) = ends edge
      attr = attribute edge

  getCommand :: Show a => String -> a -> String
  getCommand cmdName attrs = cmdName ++ show attrs

  isQueried :: String -> Graph -> Bool
  isQueried cmdName g = Set.member cmdName (queried g)

  bind :: VertexId -> VertexId -> AttributeName -> Graph -> Graph
  bind v1 v2 attr g
      | isQueried cmdName g = g
      | otherwise =
          addQueried cmdName
          $ if
              attr == _rho_
            then
              addEdge rhoEdge g
            else
              addEdge blackEdge
              $ addEdge orangeReverseEdge g
      where
        cmdName = getCommand "bind" (v1,v2,attr)
        rhoEdge = defaultEdge {ends = (v1, v2), attribute = _rho_, color = Orange}
        blackEdge = rhoEdge {color = Black, attribute = attr}
        orangeReverseEdge = rhoEdge {ends = (v2, v1)}

  atom :: VertexId -> VertexData -> Graph -> Graph
  atom v1 m1 g =
    g {
    vertexData = Map.insert v1 m1 (vertexData g)
  }

  _ksi_ = "_ksi_"

  specialLambda :: String -> String
  specialLambda m = "R(" ++ _ksi_ ++ ".t," ++ m ++ ",s)"

  tAttribute :: String
  tAttribute = "t"

  addQueried :: String -> Graph -> Graph
  addQueried cmdName g =
    g {
    queried = Set.insert cmdName (queried g)
  }

  dot :: EdgeId -> AttributeName -> EdgeId -> Graph -> Graph
  dot e1 m e2 g
    | isQueried cmdName g = g
    | otherwise =
        addQueried cmdName
        $ deleteEdge e1
        $ addEdge similarEdge
        $ addEdge tEdge
        $ atom v3 (specialLambda m)
        $ add g
    where
      edge1 = getEdge e1 g
      (v1, v2) = ends edge1
      v3 = vertexCount g
      similarEdge = edge1 {ends = (v1, v3)}
      tEdge = defaultEdge {ends = (v3, v2), attribute = tAttribute}
      cmdName = getCommand "dot" (e1, m, v3, e2)

  copy :: EdgeId -> VertexId -> EdgeId -> Graph -> Graph
  copy e1 v3 e2 g
      | isQueried (getCommand "copy" (e1, v3, e2)) g = g
      | otherwise =
            addQueried cmdName
            $ deleteEdge e1
            $ addEdge edge
            $ addEdge similarEdge g
      where
        edge1 = getEdge e1 g
        (v1, v2) = ends edge1
        similarEdge = edge1 {ends = (v1, v3)}
        edge = defaultEdge {ends = (v3, v2), color = Blue}
        cmdName = getCommand "copy" (e1, v3, e2, g)


  type Locator = String

  getIdentifiers :: Locator -> [String]
  getIdentifiers l
    | l == "" = []
    | otherwise = fst split : (getIdentifiers $ tail' $ snd split)
    where
      split = span (\c -> not (c == '.')) l
      tail' (x:xs) = xs
      tail' [] = []

  _Phi_ = "_Phi_"

  type Identifiers = [String]

  defaultVertexId :: VertexId
  defaultVertexId = -1

  getAttributeVertex :: VertexId -> AttributeName -> Graph -> VertexId
  getAttributeVertex v attr g =
    Map.findWithDefault defaultVertexId (v, attr) (attributeVertex g)

  getParentVertex :: VertexId -> Graph -> VertexId
  getParentVertex v g = getAttributeVertex v _rho_ g

  findByIdentifiers :: VertexId -> Identifiers -> Graph -> VertexId
  findByIdentifiers v [] g = v
  findByIdentifiers v (id:ids) g
      | id == _Phi_ =
          findByIdentifiers 0 ids g
      | id == _rho_ =
          findByIdentifiers (getParentVertex v g) ids g
      | otherwise =
          findByIdentifiers (getAttributeVertex v id g) ids g

  locate :: VertexId -> Locator -> Graph -> VertexId
  locate v l g = findByIdentifiers v (getIdentifiers l) g


  -- TODO color = Green?
  ref :: VertexId -> Locator -> AttributeName -> Graph -> Graph
  ref v1 l a g
      | isQueried cmdName g = g
      | otherwise =
          addQueried cmdName
          $ addEdge edge g
      where
        cmdName = getCommand "ref" (e1, v1, l, a)
        v2 = locate v1 l g
        e1 = edgeCount g
        edge = defaultEdge {ends = (v1, v2), attribute = a, specialLabel = l}

  commands g =
    dot 3 "m" (edgeCount g)
    $ ref 2 (_Phi_++".memory") "price"
    $ bind 2 4 "title"
    $ add
    $ bind 2 3 "isbn"
    $ add
    $ bind 0 2 "book2"
    $ add
    $ bind 0 1 "memory"
    $ atom 1 "M1"
    $ add
    $ add g

  -- commands emptyGraph
