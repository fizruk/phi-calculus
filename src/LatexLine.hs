module LatexLine (latexLine) where

import Data.List (intercalate, intersperse, tails)
import LatexConstants as LC
  ( ksi,
    lambda,
    llbracket,
    mapsTo,
    phi,
    rho,
    rrbracket,
    upPhi,
  )
import PhiTerms as T
  ( Mapping,
    Term (..),
  )

intercalate' :: [Char] -> [Term] -> [Char]
intercalate' sep t = intercalate sep (map latexLine t)

toStringLocator :: [Mapping] -> [Char]
toStringLocator = intercalate' "."

toStringValue :: [Term] -> [Char]
toStringValue t =
  LC.llbracket ++ intercalate' ", " t ++ LC.rrbracket

toStringSequence :: [Term] -> [Char]
toStringSequence = intercalate' ", "

latexLine :: Term -> [Char]
latexLine t =
  case t of
    (A a) ->
      case a of
        "$" -> LC.ksi
        "^" -> LC.rho
        "#" -> LC.upPhi
        "@" -> LC.phi
        _ -> a
    (L l) ->
      LC.lambda ++ tails l !! 3
    ((A a) `App` [value]) ->
      latexLine (A a) ++ " ( " ++ toStringValue value ++ " ) "
    (M (A a) attributes [value]) ->
      latexLine (A a)
        ++ case attributes of
          [] -> ""
          _ -> " ( " ++ toStringSequence attributes ++ " ) "
        ++ mapsTo
        ++ toStringValue value
    ((A a) `ToLocator` locator) ->
      latexLine (A a) ++ mapsTo ++ toStringLocator locator
    ((A a) `ToLambda` l) ->
      latexLine (A a) ++ mapsTo ++ latexLine l
    _ -> ""