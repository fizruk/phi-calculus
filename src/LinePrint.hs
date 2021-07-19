module LinePrint(linePrint) where

import PhiTerms as T
    ( Term(..),
      Attribute,
      mapsTo,
      ksiPretty,
      rhoPretty,
      upPhiPretty,
      phiPretty )
import Data.List (intercalate, intersperse, tails)

intercalate' :: [Char] -> [Term] -> [Char]
intercalate' sep t = intercalate sep (map linePrint t)

toStringLocator :: [Attribute] -> [Char]
toStringLocator = intercalate' "."

toStringValue :: [Term] -> [Char]
toStringValue t =
  " \\llbracket "
    ++ intercalate' ", " t
    ++ " \\rrbracket "

toStringSequence :: [Term] -> [Char]
toStringSequence = intercalate' ", "


linePrint :: Term -> [Char]
linePrint t =
  case t of
    (A a) ->
      case a of
        "$" -> T.ksiPretty
        "^" -> T.rhoPretty
        "#" -> T.upPhiPretty
        "@" -> T.phiPretty
        _ -> a
    (L l) ->
      "\\lambda s." ++ tails l !! 3
    ((A a) `App` [value]) ->
      linePrint (A a) ++ " ( " ++ toStringValue value ++ " ) "
    (Abstract (A a) attributes [value]) ->
      linePrint (A a) ++ " ( " ++ toStringSequence attributes ++ " ) " ++ mapsTo ++ toStringValue value
    (Closed (A a) [value]) ->
      linePrint (A a) ++ mapsTo ++ toStringValue value
    ((A a) `ToLocator` locator) ->
      linePrint (A a) ++ mapsTo ++ toStringLocator locator
    ((A a) `ToLambda` l) ->
      linePrint (A a) ++ mapsTo ++ linePrint l
    _ -> ""