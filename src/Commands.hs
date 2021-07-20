module Commands(Command(..)) where

import LatexConstants(quad)
import Text.Printf (printf)

type VertexId = Int

type EdgeId = Int

type LambdaBody = String

type AttributeName = String

type Locator = String

data Command
  = ADD VertexId
  | BIND VertexId VertexId AttributeName
  | DOT EdgeId AttributeName VertexId EdgeId
  | COPY EdgeId VertexId EdgeId
  | ATOM VertexId LambdaBody
  | REF EdgeId VertexId Locator AttributeName
  deriving (Eq, Ord)

instance Show Command where
  show t =
    printf " \\mathbf { %s } " s ++ quad
    where
      s =
        case t of
          ADD v -> printf " ADD(v_%d) " v
          BIND v1 v2 a -> printf " BIND(v_%d, v_%d, %s) " v1 v2 a :: String
          DOT e1 a v e2 -> printf " DOT(e_%d, %s, v_%d, e_%d) " e1 a v e2
          COPY e1 v e2 -> printf " COPY(e_%d, v_%d, e_%d) " e1 v e2
          ATOM v l -> printf " ATOM(v_%d, \\lambda.%s) " v l
          REF e v l a -> printf " REF(e_%d, v_%d, %s, %s) " e v l a
