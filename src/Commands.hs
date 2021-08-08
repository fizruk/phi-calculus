module Commands (Command (..)) where

import qualified LatexConstants as LC (lambdaS, quad)
import LatexLine (latexLine)
import PhiGrammar (AttributeName, AttributeNameOrCopy, Lambda, Term (..))
import Text.Printf (printf)

type VertexId = Int

type EdgeId = Int

data Command
  = ADD VertexId
  | BIND VertexId VertexId AttributeName
  | COPY VertexId VertexId AttributeName
  | ATOM VertexId AttributeName VertexId Lambda

instance Show Command where
  show t =
    printf " \\mathbf { %s } " s ++ LC.quad
    where
      s =
        case t of
          ADD v -> printf " ADD(v_{%d}) " v
          BIND v1 v2 a -> printf " BIND(v_{%d}, v_{%d}, %s) " v1 v2 (latexLine a)::String 
          COPY v1 v2 a -> printf " COPY(v_{%d}, v_{%d}, %s) " v1 v2 (latexLine a)::String
          ATOM v1 a v2 sM ->
            printf " ATOM(v_{%d}, %s, v_{%d}, %s) " v1 (latexLine a) v2 (latexLine sM)