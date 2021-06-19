module Graph
    where
      import qualified Data.Map as Map
      import Data.Set (Set)
      import qualified Data.Set as Set

      -- lambda expression attached to a vertex
      type Lam = Maybe String

      -- vertices are just numbers
      type Vertex = Int

      -- color of an edge
      data EdgeColor = Black | Blue | Orange deriving (Show, Eq)

      -- attribute that this edge denotes
      type EdgeLabel = String

      -- color, attribute
      data Edge =
        Edge {
          edgeColor :: EdgeColor,
          edgeLabel :: EdgeLabel,
          edgeSupplementaryLabel :: EdgeLabel
        } deriving (Show)

      -- find info about an edge with two given vertices

      data EdgeEnds =
        EdgeEnds {
          from :: Vertex,
          to :: Vertex
        } deriving (Show, Ord, Eq)


      -- just mappings between vertices
      -- all vertices are guarranteed to be unique
      data Graph =
        Graph {
          neighs :: Map.Map Vertex (Set Vertex),
          edges :: Map.Map EdgeEnds Edge
        } deriving(Show)


      emptyGraph :: Graph
      emptyGraph =
        Graph {
          neighs = Map.empty,
          edges = Map.empty
        }

      add :: Vertex -> Graph -> Graph
      add v (Graph {neighs = neighs, edges = edges}) =
        Graph {
          neighs = case Map.lookup v neighs of
            Just _  -> neighs
            Nothing -> Map.insert v Set.empty neighs
          ,
          edges = edges
        }
      --
      addEdge :: EdgeEnds -> Graph -> Graph
      addEdge (EdgeEnds v_1 v_2) (Graph {neighs = neighs, edges = edges}) =
        Graph {
          neighs = case Map.lookup v_1 neighs of
            Just s -> Map.insert v_1 s neighs
            Nothing -> neighs
          ,
          edges = edges
      }

      addEdge' :: Vertex -> Vertex -> Graph -> Graph
      addEdge' v_1 v_2 = addEdge (EdgeEnds v_1 v_2)

      addEdgeLabel :: EdgeEnds -> EdgeLabel -> Graph -> Graph
      addEdgeLabel ends label (Graph {neighs = neighs, edges = edges}) =
        Graph {
          neighs = neighs
          ,
          edges =
            let found = Map.lookup ends edges
                insertRho =
                  Map.insert
                    ends
                    (Edge {edgeColor = Orange, edgeLabel = label,edgeSupplementaryLabel=""})
                    edges
                insertReverseOrange =
                  Map.insert
                  (EdgeEnds (to ends) (from ends))
                  (Edge {edgeColor = Orange, edgeLabel = "𝜌",edgeSupplementaryLabel=""})
                  edges
                insertBlack =
                  Map.insert
                  ends
                  (Edge {edgeColor = Black, edgeLabel = label, edgeSupplementaryLabel=""})
            in
              case found of
                Just _ -> edges
                Nothing ->
                  case label of
                    "𝜌" -> insertRho
                    otherwise ->
                       insertBlack insertReverseOrange
      }

      addEdgeLabel' :: Vertex -> Vertex -> EdgeLabel -> Graph -> Graph
      addEdgeLabel' v_1 v_2 = addEdgeLabel (EdgeEnds v_1 v_2)

      bind :: Vertex -> Vertex -> EdgeLabel -> Graph -> Graph
      bind v_1 v_2 label g =
        addEdgeLabel' v_1 v_2 label $
        addEdge' v_1 v_2 g

      






      -- ? как изменить конкретное поле "объекта"
