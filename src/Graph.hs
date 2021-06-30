{-# LANGUAGE LambdaCase #-}
module Graph where

-- TODO implement graph with search adjacent by vertex index and attr name
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

import           Data.Function   ((&))

type VertexId = Int

type AttributeName = String

-- | Lambdas?
type VertexData = String

type EdgeId = Int

-- | Secondary edge label.
type SpecialLabel = String

data EdgeType
  = AttributeEdge
  | ParentEdge
  | CopyEdge
  deriving (Show)

type EdgeEnds = (VertexId, VertexId)

data Edge = Edge
  { ends         :: EdgeEnds
  , attribute    :: AttributeName
  , specialLabel :: SpecialLabel
  , edgeType        :: EdgeType
  } deriving (Show)

data Graph = Graph
  { vertexData      :: Map.Map VertexId (Maybe VertexData)
  , edgeData        :: Map.Map EdgeId Edge
  , attributeVertex :: Map.Map (VertexId, AttributeName) VertexId
  , vertexCount     :: Int
  , edgeCount       :: Int
  , queried         :: Set.Set Command
    -- ^ Record already executed GMIs to preserve idempotent property.
    -- FIXME: check that this is correct.
  } deriving (Show)

data Command
  = ADD
  | BIND VertexId VertexId AttributeName
  | DOT EdgeId AttributeName EdgeId
  | COPY EdgeId VertexId EdgeId
  | ATOM VertexId VertexData
  | REF VertexId LocatorString AttributeName
  deriving (Eq, Ord, Show)

executeCommand :: Command -> (Graph -> Graph)
executeCommand cmd graph
  | isQueried cmd graph = graph
  | otherwise = addQueried cmd (modifier graph)
  where
    modifier = case cmd of
      ADD          -> add
      BIND v1 v2 a -> bind v1 v2 a
      DOT e1 a e2  -> dot e1 a e2
      COPY e1 v e2 -> copy e1 v e2
      ATOM v m     -> atom v m
      REF v l a    -> ref v l a

executeCommands :: [Command] -> Graph -> Graph
executeCommands []           = id
executeCommands (cmd : cmds) = executeCommands cmds . executeCommand cmd

defaultEdge :: Edge
defaultEdge = Edge
  { ends = (-1, -1)
  , attribute = ""
  , specialLabel = ""
  , edgeType = AttributeEdge
  }

emptyGraph :: Graph
emptyGraph = Graph
  { vertexData = Map.empty
  , edgeData = Map.empty
  , attributeVertex = Map.empty
  , vertexCount = 0
  , edgeCount = 0
  , queried = Set.empty
  }

add :: Graph -> Graph
add g = g
  { vertexData = Map.insert (vertexCount g) Nothing (vertexData g)
  , vertexCount = (vertexCount g) + 1
  }

getEdge :: EdgeId -> Graph -> Edge
getEdge edgeId g = Map.findWithDefault defaultEdge edgeId (edgeData g)

_rho_ :: AttributeName
_rho_ = "_rho_"

addEdge :: Edge -> Graph -> Graph
addEdge e g = g
  { edgeData = Map.insert (edgeCount g) e (edgeData g)
  , edgeCount = (edgeCount g) + 1
  , attributeVertex = Map.insert (v1, attr) v2 (attributeVertex g)
  }
  where
    (v1, v2) = ends e
    attr = attribute e

deleteEdge :: EdgeId -> Graph -> Graph
deleteEdge e g = g
  { edgeData = Map.delete e (edgeData g)
    , attributeVertex = Map.delete (v1, attr) $ attributeVertex g
  }
  where
    edge = getEdge e g
    (v1, _) = ends edge
    attr = attribute edge

getCommand :: Show a => String -> a -> String
getCommand cmdName attrs = cmdName ++ show attrs

isQueried :: Command -> Graph -> Bool
isQueried cmd g = cmd `Set.member` queried g

bind :: VertexId -> VertexId -> AttributeName -> Graph -> Graph
bind v1 v2 attr g
  | attr == _rho_ = addEdge rhoEdge g
  | otherwise     = addEdge blackEdge (addEdge orangeReverseEdge g)
  where
    rhoEdge = defaultEdge {ends = (v1, v2), attribute = _rho_, edgeType = ParentEdge}
    blackEdge = rhoEdge {edgeType = AttributeEdge, attribute = attr}
    orangeReverseEdge = rhoEdge {ends = (v2, v1)}

atom :: VertexId -> VertexData -> Graph -> Graph
atom v1 m1 g = g {vertexData = Map.insert v1 (Just m1) (vertexData g)}

_xi_ :: AttributeName
_xi_ = "_xi_"

specialLambda :: String -> String
specialLambda m = "R(" ++ _xi_ ++ ".t," ++ m ++ ",s)"

tAttribute :: String
tAttribute = "t"

addQueried :: Command -> Graph -> Graph
addQueried ADD g = g
addQueried cmd g = g {queried = Set.insert cmd (queried g)}

dot :: EdgeId -> AttributeName -> EdgeId -> Graph -> Graph
dot e1 m e2 g = g
  & add
  & atom v3 (specialLambda m)
  & addEdge tEdge
  & addEdge similarEdge
  & deleteEdge e1
  where
    edge1       = getEdge e1 g
    (v1, v2)    = ends edge1
    v3          = vertexCount g
    similarEdge = edge1 {ends = (v1, v3)}
    tEdge       = defaultEdge {ends = (v3, v2), attribute = tAttribute}

copy :: EdgeId -> VertexId -> EdgeId -> Graph -> Graph
copy e1 v3 e2 g = g
  & addEdge similarEdge
  & addEdge edge
  & deleteEdge e1
  where
    edge1       = getEdge e1 g
    (v1, v2)    = ends edge1
    similarEdge = edge1 {ends = (v1, v3)}
    edge        = defaultEdge {ends = (v3, v2), edgeType = CopyEdge}


type LocatorString = String

getIdentifiers :: LocatorString -> [String]
getIdentifiers l
  | l == ""   = []
  | otherwise = fst split : (getIdentifiers (tail' (snd split)))
  where
    split = span (\c -> not (c == '.')) l

tail' :: [a] -> [a]
tail' (x:xs) = xs
tail' []     = []

_Phi_ :: AttributeName
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
    | otherwise   =
        findByIdentifiers (getAttributeVertex v id g) ids g

locate :: VertexId -> LocatorString -> Graph -> VertexId
locate v l g = findByIdentifiers v (getIdentifiers l) g

ref :: VertexId -> LocatorString -> AttributeName -> Graph -> Graph
ref v1 l a g = addEdge edge g
  where
    v2   = locate v1 l g
    e1   = edgeCount g
    edge = defaultEdge {ends = (v1, v2), attribute = a, specialLabel = l}

executeSampleCommands :: Graph -> Graph
executeSampleCommands g = g & executeCommands
  [ ADD -- vertex 0
  , ADD -- vertex 1
  , ATOM 1 "M1"
  , BIND 0 1 "memory"
  , ADD -- vertex 2
  , BIND 0 2 "book2"
  , ADD -- vertex 3
  , BIND 2 3 "isbn"
  , ADD -- vertex 4
  , BIND 2 4 "title"
  , REF 2 (_Phi_++".memory") "price"
  , DOT 3 "m" (edgeCount g)
  ]

-- commands emptyGraph

_phi_ :: AttributeName
_phi_ = "_phi_"

data Locator
  = Empty
  | UpperPhi
  | LowerPhi
  | Rho
  | Xi
  | Attr AttributeName
  | Locator `Dot` Locator
  deriving (Show, Eq, Ord)

-- translate locator identifier to string
-- locatorIdToString :: Locator -> LocatorString
-- locatorIdToString l =


locatorToString :: Locator -> LocatorString
locatorToString l =
  case l of
    x `Dot` xs -> (locatorToString x) ++ "." ++ (locatorToString xs)
    otherwise -> locatorIdToString
  where
    locatorIdToString =
      case l of
        UpperPhi  -> _Phi_
        LowerPhi  -> _phi_
        Rho       -> _rho_
        Xi        -> _xi_
        Attr m    -> m
        otherwise -> "_|_"
