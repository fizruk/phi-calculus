module PhiTerms
  ( Term (..),
    Attribute,
    mapsTo,
    ksi,
    ksiPretty,
    rho,
    rhoPretty,
    upPhi,
    upPhiPretty,
    phi,
    phiPretty,
  )
where

import Data.List (intercalate, intersperse, tails)

-- Term Grammar
type AttributeName = Term

type L = Term

-- non-terminals
type Object = Term

type FreeAttributes = [AttributeName]

type Attribute = Term

type Value = [[Attribute]]

type Locator = [Attribute]

-- instead of signature of an abstract object
type AbstractAttribute = Term

-- | includes special characters
type AnyName = Term

data Term
  = -- | \s.M
    -- | L == Lambda
    L String
  | -- | names excluding special characters
    A String
  | -- |  a([b>c, d>[e]])
    -- | App == value is Applied to object
    AttributeName `App` Value
  | -- | a [b, c] > [d>e]
    -- | ATo == Abstract object maps to
    Abstract AttributeName FreeAttributes Value
  | -- | a > [b>c]. Any because of φ
    -- | Closed == Closed object maps To
    Closed AttributeName Value
  | -- | a > b.c
    -- | ToLocator == attribute name maps To Locator
    AttributeName `ToLocator` Locator
  | -- | a > \s.M
    -- | ToLambda == attribute name maps To Lambda
    AttributeName `ToLambda` L

-- LaTeX representation of terms
mapsTo = " \\mapsto "
ksi = "$"
rho = "^"
upPhi = "#"
phi = "@"
ksiPretty = " \\ksi "
rhoPretty = " \\rho "
upPhiPretty = " \\upPhi "
phiPretty = " \\varphi "

-- type Depth = Int
-- prettyPrint :: Term -> Depth -> [Char]
-- prettyPrint

{--
instance Show Term where
    show (A a) =
        case a of
            "$" -> " \\ksi "
            "^" -> " \\rho "
            "#" -> " \\upPhi "
            "@" -> " \\varphi "
            _ -> a

    show (L l) = "\\lambda s." ++ tails l !! 3

    show ((A a) `App` [value]) =
        show (A a) ++ " ( " ++ toStringValue value ++ " ) "

    show (Abstract (A a) attributes [value]) =
        show (A a) ++ " ( " ++ toStringSequence attributes ++ " ) " ++ mapsTo ++ toStringValue value

    show (Closed (A a) [value]) =
        show (A a) ++ mapsTo ++ toStringValue value

    show ((A a) `ToLocator` locator) =
        show (A a) ++ mapsTo ++ toStringLocator locator

    show ((A a) `ToLambda` l) =
        show (A a) ++ mapsTo ++ show l

    show _ = ""
--}

-- type VertexId = Int
-- type EdgeId = Int

-- data Command
--   = ADD VertexId
--   | BIND VertexId VertexId AttributeName
--   | DOT EdgeId AttributeName VertexId EdgeId
--   | COPY EdgeId VertexId EdgeId
--   | ATOM VertexId L
--   | REF EdgeId VertexId Locator AttributeName
--   deriving (Show)

-- instance Eq Command where
--   x == y = show x == show y

-- instance Ord Command where
--   x `compare` y = show x `compare` show y

-- data State = State
--   { focusedElementIndex :: Int,
--     currentPremiseString :: String,
--     latexedPremise :: String,
--     latexedConclusion :: String,
--     latexedRule :: String,
--     -- | increment only
--     edgeCounter :: Int,
--     -- | increment only
--     vertexCounter :: Int,
--     -- | decrement only
--     dataCounter :: Int,
--     gmis :: [Command]
--   }

-- emptyState :: State
-- emptyState =
--   State
--     { focusedElementIndex = 0,
--       currentPremiseString = "",
--       latexedPremise = "",
--       latexedConclusion = "",
--       latexedRule = "",
--       edgeCounter = 0,
--       vertexCounter = 0,
--       dataCounter = -1,
--       gmis = []
--     }

-- r0 :: Term -> State -> State
-- r0 t s =

-- r1 :: Term -> State -> State
-- r1 t s =

--     where
