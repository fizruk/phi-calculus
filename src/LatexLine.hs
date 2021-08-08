module LatexLine (latexLine, toStringSequence, toStringValue, toStringLocator) where

import Data.List (intercalate, intersperse, tails)
import qualified LatexConstants as LC
  ( ksi,
    lambdaS,
    llbracket,
    mapsTo,
    phi,
    quad,
    rho,
    rrbracket,
    upPhi,
  )
import PhiGrammar as T
  ( Mapping,
    Term (..),
  )

intercalate' :: [Char] -> [Term] -> [Char]
intercalate' sep t = intercalate sep (map latexLine t)

toStringLocator :: [Mapping] -> [Char]
toStringLocator = intercalate' "."

toStringValue :: [Term] -> [Char]
toStringValue t =
  LC.llbracket ++ toStringSequence t ++ LC.rrbracket

toStringSequence :: [Term] -> [Char]
toStringSequence = intercalate' (", " ++ LC.quad)

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
      LC.lambdaS ++ l
    ((A a) `App` [value]) ->
      latexLine (A a) ++ " ( " ++ toStringValue value ++ " ) "
    (M (A a) attributes [value]) ->
      latexLine (A a)
        ++ case attributes of
          [] -> ""
          _ -> " ( " ++ toStringSequence attributes ++ " ) "
        ++ LC.mapsTo
        ++ toStringValue value
    ((A a) `ToLocator` locator) ->
      latexLine (A a) ++ LC.mapsTo ++ toStringLocator locator
    ((A a) `ToLambda` l) ->
      latexLine (A a) ++ LC.mapsTo ++ latexLine l
    _ -> ""