module Commands(Command(..)) where

import qualified LatexConstants as LC(quad, lambdaS)
import Text.Printf (printf)
import PhiTerms (Term(..), AttributeName, Lambda, AttributeNameOrCopy)
import LatexLine (latexLine)

type VertexId = Int

type EdgeId = Int

data Command
  = ADD VertexId
  | BIND VertexId VertexId AttributeName
  | DOT EdgeId AttributeName VertexId EdgeId
  | COPY EdgeId VertexId EdgeId
  | ATOM VertexId Lambda
  | REF EdgeId VertexId Term AttributeName

instance Show Command where
  show t =
    printf " \\mathbf { %s } " s ++ LC.quad
    where
      s =
        case t of
          ADD v -> printf " ADD(v_{%d}) " v
          BIND v1 v2 (A a) -> printf " BIND(v_{%d}, v_{%d}, %s) " v1 v2 (latexLine (A a))::String
          DOT e1 (A a) v e2 -> printf " DOT(e_{%d}, %s, v_{%d}, e_{%d}) " e1 (latexLine (A a)) v e2
          COPY e1 v e2 -> printf " COPY(e_{%d}, v_{%d}, e_{%d}) " e1 v e2
          ATOM v sM -> 
            printf " ATOM(v_{%d}, %s) " v m
            where 
              L m = sM
          REF e v l (A a) -> printf " REF(e_{%d}, v_{%d}, %s, %s) " e v (latexLine l) (latexLine (A a))
          _ -> error "Incorrect GMI"

