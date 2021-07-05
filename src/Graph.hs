module Graph where

-- TODO implement graph with search adjacent by vertex index and attr name

import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type VertexId = Int

type AttributeName = String

data Attribute
  = UpperPhi
  | LowerPhi
  | Rho
  | Xi
  | T
  | Copy
  | Attr AttributeName

instance Show Attribute where
  show UpperPhi = "_Phi_"
  show LowerPhi = "_phi_"
  show Rho = "_rho_"
  show Xi = "_xi_"
  show T = "_t_"
  show Copy = "_copy_"
  show (Attr a) = show a

instance Eq Attribute where
  x == y = show x == show y

instance Ord Attribute where
  x `compare` y = show x `compare` show y

type VertexData = String

type EdgeId = Int

type Locator = [Attribute]

-- | Secondary edge label.
type SpecialLabel = Locator

data EdgeType
  = AttributeEdge
  | ParentEdge
  | CopyEdge
  deriving (Show)

type EdgeEnds = (VertexId, VertexId)

data Edge = Edge
  { ends :: EdgeEnds,
    attribute :: Attribute,
    specialLabel :: Locator,
    edgeType :: EdgeType
  }
  deriving (Show)

defaultEdge :: Edge
defaultEdge =
  Edge
    { ends = (-1, -1),
      attribute = Attr "",
      specialLabel = [],
      edgeType = ParentEdge
    }

data Graph = Graph
  { vertexData :: Map.Map VertexId VertexData,
    edgeData :: Map.Map EdgeId Edge,
    attributeVertex :: Map.Map (VertexId, Attribute) VertexId,
    vertexCount :: Int,
    edgeCount :: Int,
    -- | Record already executed GMIs to preserve idempotent property.
    -- FIXME: check that this is correct.
    queried :: Set.Set Command
  }
  deriving (Show)

data Command
  = ADD
  | BIND VertexId VertexId Attribute
  | DOT EdgeId Attribute EdgeId
  | COPY EdgeId VertexId EdgeId
  | ATOM VertexId VertexData
  | REF VertexId Locator Attribute
  deriving (Eq, Ord, Show)

executeCommand :: Command -> (Graph -> Graph)
executeCommand cmd graph
  | isQueried cmd graph = graph
  | otherwise = addQueried cmd (modifier graph)
  where
    modifier = case cmd of
      ADD -> add
      BIND v1 v2 a -> bind v1 v2 a
      DOT e1 a e2 -> dot e1 a e2
      COPY e1 v e2 -> copy e1 v e2
      ATOM v m -> atom v m
      REF v l a -> ref v l a

executeCommands :: [Command] -> Graph -> Graph
executeCommands [] = id
executeCommands (cmd : cmds) = executeCommands cmds . executeCommand cmd

emptyGraph :: Graph
emptyGraph =
  Graph
    { vertexData = Map.empty,
      edgeData = Map.empty,
      attributeVertex = Map.empty,
      vertexCount = 0,
      edgeCount = 0,
      queried = Set.empty
    }

add :: Graph -> Graph
add g =
  g
    { vertexCount = vertexCount g + 1
    }

getEdge :: EdgeId -> Graph -> Edge
getEdge edgeId g = Map.findWithDefault defaultEdge edgeId (edgeData g)

addEdge :: Edge -> Graph -> Graph
addEdge e g =
  g
    { edgeData = Map.insert (edgeCount g) e (edgeData g),
      edgeCount = edgeCount g + 1,
      attributeVertex = Map.insert (v1, attr) v2 (attributeVertex g)
    }
  where
    (v1, v2) = ends e
    attr = attribute e

deleteEdge :: EdgeId -> Graph -> Graph
deleteEdge e g =
  g
    { edgeData = Map.delete e (edgeData g),
      attributeVertex = Map.delete (v1, attr) $ attributeVertex g
    }
  where
    edge = getEdge e g
    (v1, _) = ends edge
    attr = attribute edge

getCommand :: Show a => String -> a -> String
getCommand cmdName attrs = cmdName ++ show attrs

isQueried :: Command -> Graph -> Bool
isQueried cmd g = cmd `Set.member` queried g

bind :: VertexId -> VertexId -> Attribute -> Graph -> Graph
bind v1 v2 attr g
  | attr == Rho = addEdge rhoEdge g
  | otherwise = addEdge blackEdge (addEdge orangeReverseEdge g)
  where
    rhoEdge = defaultEdge {ends = (v1, v2), attribute = Rho, edgeType = ParentEdge}
    blackEdge = rhoEdge {edgeType = AttributeEdge, attribute = attr}
    orangeReverseEdge = rhoEdge {ends = (v2, v1)}

atom :: VertexId -> VertexData -> Graph -> Graph
atom v1 m1 g = g {vertexData = Map.insert v1 m1 (vertexData g)}

specialLambda :: Attribute -> String
specialLambda m = "discover(" ++ show Xi ++ "." ++ show T ++ ", " ++ show m ++ ", s)"

addQueried :: Command -> Graph -> Graph
addQueried ADD g = g
addQueried cmd g = g {queried = Set.insert cmd (queried g)}

dot :: EdgeId -> Attribute -> EdgeId -> Graph -> Graph
dot e1 m e2 g =
  g
    & add
    & atom v3 (specialLambda m)
    & addEdge tEdge
    & addEdge similarEdge
    & deleteEdge e1
  where
    edge1 = getEdge e1 g
    (v1, v2) = ends edge1
    v3 = vertexCount g
    similarEdge = edge1 {ends = (v1, v3)}
    tEdge = defaultEdge {ends = (v3, v2), attribute = T}

copy :: EdgeId -> VertexId -> EdgeId -> Graph -> Graph
copy e1 v3 e2 g =
  g
    & addEdge similarEdge
    & addEdge edge
    & deleteEdge e1
  where
    edge1 = getEdge e1 g
    (v1, v2) = ends edge1
    similarEdge = edge1 {ends = (v1, v3)}
    edge = defaultEdge {ends = (v3, v2), attribute = Copy, edgeType = CopyEdge}

type LocatorString = String

tail' :: [a] -> [a]
tail' (x : xs) = xs
tail' [] = []

type Identifiers = [String]

defaultVertexId :: VertexId
defaultVertexId = -1

getAttributeVertex :: VertexId -> Attribute -> Graph -> VertexId
getAttributeVertex v attr g =
  Map.findWithDefault defaultVertexId (v, attr) (attributeVertex g)

getParentVertex :: VertexId -> Graph -> VertexId
getParentVertex v = getAttributeVertex v Rho

findByLocator :: VertexId -> Locator -> Graph -> VertexId
findByLocator v [] g = v
findByLocator v (id : ids) g
  | id == UpperPhi =
    findByLocator 0 ids g
  | id == Rho =
    findByLocator (getParentVertex v g) ids g
  | otherwise =
    findByLocator (getAttributeVertex v id g) ids g

ref :: VertexId -> Locator -> Attribute -> Graph -> Graph
ref v1 l a g = addEdge edge g
  where
    v2 = findByLocator v1 l g
    e1 = edgeCount g
    edge = defaultEdge {ends = (v1, v2), attribute = a, specialLabel = l}

executeSampleCommands :: Graph -> Graph
executeSampleCommands g =
  g
    & executeCommands
      [ ADD, -- vertex 0
        ADD, -- vertex 1
        ATOM 1 "M1",
        BIND 0 1 (Attr "memory"),
        ADD, -- vertex 2
        BIND 0 2 (Attr "book2"),
        ADD, -- vertex 3
        BIND 2 3 (Attr "isbn"),
        ADD, -- vertex 4
        BIND 2 4 (Attr "title"),
        REF 2 [UpperPhi, Attr ".memory"] (Attr "price"),
        DOT 3 (Attr "m") (edgeCount g)
      ]

{-
>>> executeSampleCommands emptyGraph
Graph {vertexData = fromList [(1,"M1"),(5,"R(_xi_.t,\"m\",s)")], edgeData = fromList [(0,Edge {ends = (1,0), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(1,Edge {ends = (0,1), attribute = "memory", specialLabel = [], edgeType = AttributeEdge}),(2,Edge {ends = (2,0), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(4,Edge {ends = (3,2), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(5,Edge {ends = (2,3), attribute = "isbn", specialLabel = [], edgeType = AttributeEdge}),(6,Edge {ends = (4,2), attribute = _rho_, specialLabel = [], edgeType = ParentEdge}),(7,Edge {ends = (2,4), attribute = "title", specialLabel = [], edgeType = AttributeEdge}),(8,Edge {ends = (2,-1), attribute = "price", specialLabel = [_Phi_,".memory"], edgeType = ParentEdge}),(9,Edge {ends = (5,2), attribute = t, specialLabel = [], edgeType = ParentEdge}),(10,Edge {ends = (0,5), attribute = "book2", specialLabel = [], edgeType = AttributeEdge})], attributeVertex = fromList [((0,"memory"),1),((1,_rho_),0),((2,"isbn"),3),((2,"price"),-1),((2,"title"),4),((2,_rho_),0),((3,_rho_),2),((4,_rho_),2),((5,t),2)], vertexCount = 6, edgeCount = 11, queried = fromList [BIND 0 1 "memory",BIND 0 2 "book2",BIND 2 3 "isbn",BIND 2 4 "title",DOT 3 "m" 0,ATOM 1 "M1",REF 2 [_Phi_,".memory"] "price"]}
-}

stop :: [Attribute]
stop = [Attr "_|_"]

evaluateLambda :: VertexData -> Vector -> Locator
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
    m = Map.findWithDefault "" w (vertexData g)
    hasAAttribute = getAttributeVertex w a g /= defaultVertexId
    hasLowerPhiAttribute = getAttributeVertex w LowerPhi g /= defaultVertexId
    hasCopyAttribute = getAttributeVertex w Copy g /= defaultVertexId
    hasLambda = m /= ""
