{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Translator where

import Data.Maybe
import Graph
import Phi

exampleTerm :: Term String
exampleTerm =
  Object
    [ ( AttrIdent "book",
        Just
          ( Object
              [ (AttrIdent "isbn", Nothing),
                ( AttrIdent "title",
                  Just
                    ( Object
                        [(AttrDelta, Just (Data "Sample title"))]
                    )
                )
              ]
          )
      )
    ]

getAttr :: Term String -> Attr
getAttr t =
  case t of
    Object a -> fst (head a) -- get attribute of the object
    Data str -> AttrIdent _delta_

getSubTerm :: Term String -> Term String
getSubTerm t =
  case t of
    Object ((_, Nothing) : a) -> error "Term have no subterms"
    Object a -> fromJust $ snd (head a) -- get subterm of the object
    _ -> undefined

attrToStr :: Attr -> String
attrToStr attr =
  case attr of
    AttrPhi -> "_Phi_"
    AttrDelta -> "_Delta_"
    AttrRho -> "_Rho_"
    AttrIdent id -> id :: String

traverseGraph :: Term String -> [Attr]
traverseGraph term =
  case term of
    Object (subterm : subterms)
      | not (null subterms) ->
        traverseGraph (Object [subterm]) ++ traverseGraph (Object subterms)
    Object [(_, Just a)] ->
      getAttr term : traverseGraph (getSubTerm term)
    Object [(_, Nothing)] ->
      [getAttr term]
    Data str -> [getAttr term]
    _ -> []

book :: Term String
book =
  Object
    [ ( AttrIdent "book2",
        Just
          ( Object
              [ (AttrIdent "sibn", Nothing),
                (AttrIdent "title", Just (Data "some data"))
              ]
          )
      )
    ]

phiGraph :: Graph
phiGraph = add emptyGraph --initial graph with main(big Phi) vertex

translation :: Term String -> Graph -> Graph
translation term g =
  case term of
    Object [(_, Nothing)] -> 
      bind (vertexCount newG) (vertexCount newG - 1) (attrToStr $ getAttr term) newG
      where newG = add g
    _ -> emptyGraph