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
      add v g = g {
        neighs = case Map.lookup v (neighs g) of
          Just _  -> (neighs g)
          Nothing -> Map.insert v Set.empty (neighs g)
      }

      addEdge :: EdgeEnds -> Graph -> Graph
      addEdge (EdgeEnds v_1 v_2) g = g {
        neighs = case Map.lookup v_1 (neighs g) of
          Just s -> Map.insert v_1 s (neighs g)
          Nothing -> (neighs g)
      }

      addEdge' :: Vertex -> Vertex -> Graph -> Graph
      addEdge' v_1 v_2 = addEdge (EdgeEnds v_1 v_2)

      defaultEdge = Edge {edgeColor = Black, edgeLabel = "", edgeSupplementaryLabel = ""}

      addEdgeLabel :: EdgeEnds -> EdgeLabel -> Graph -> Graph
      addEdgeLabel ends label g = g {
        edges =
          let found = Map.lookup ends (edges g)
              insertRho =
                Map.insert
                  ends
                  (defaultEdge {edgeColor = Orange, edgeLabel = label})
                  (edges g)
              insertReverseOrange =
                Map.insert
                (EdgeEnds (to ends) (from ends))
                (defaultEdge {edgeColor = Orange, edgeLabel = label})
                (edges g)
              insertBlack =
                Map.insert
                ends
                defaultEdge {edgeLabel = label}
          in
            case found of
              Just _ -> (edges g)
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
