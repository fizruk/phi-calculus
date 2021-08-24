module PreprocessingRules where

import Data.Function ((&))
import Data.List (stripPrefix)
import LatexLine
import PhiTerms
import Text.Printf

-- preprocess::Term -> Term
-- preprocess t =

data State = State
  { term :: Term,
    list :: [Term],
    c :: AttributeName,
    d0 :: AttributeName,
    d :: Int
  }

-- | for passing only what's necessary
sEmpty :: State
sEmpty =
  State
    { term = A "",
      list = [],
      c = A "",
      d0 = A "",
      d = 0
    }

rule1 :: State -> State
rule1 sIn = sOut
  where
    t = term sIn
    sOut =
      case t of
        M name freeAttributes [e] ->
          sOut1
            { term = M name [] [addFree freeAttributes ++ list sOut1]
            }
          where
            -- P1.1
            addFree [] = []
            -- P1.2
            addFree (f : fs) = M f [] [[]] : addFree fs
            sIn1 = sEmpty {list = e}
            sOut1 = rule2 sIn1
        _ -> error (printf "P1: wrong term %s" (latexLine t))

rule2 :: State -> State
rule2 sIn = sOut
  where
    es = list sIn

    chooseRule t =
      case t of
        M {} -> rule1
        c `ToLocator` b ->
          case stripPrefix (takeTillApplication b) b of
            -- P3.1
            Nothing -> const sEmpty {list = [t]}
            -- P3.2
            _ -> rule3
        _ -> error (printf "P2: wrong term %s" (latexLine t))

    combine s t = sCombined
      where
        sNext = chooseRule t sEmpty {term = t, d = d s}
        sCombined =
          sEmpty
            { list = list s ++ list sNext,
              d = d sNext
            }

    sOut = foldl combine sIn es

-- | prefix of a locator till first application inclusively
takeTillApplication :: Locator -> Locator
takeTillApplication (x : xs) =
  case x of
    App {} -> [x]
    A {} -> x : takeTillApplication xs
    _ -> error (printf "Wrong locator identifier: %s" (latexLine x))
takeTillApplication _ = []

-- NOTE: stripPrefix
type TermList = [Term]

-- | P3.2
rule3 :: State -> State
rule3 sIn = sOut
  where
    t = term sIn
    -- c
    -- list of attribute ids (with single application at the end)
    -- rest of locator
    (c, as, e) =
      case t of
        c `ToLocator` b ->
          (c, as, e)
          where
            as = takeTillApplication b
            e = stripPrefix as b
        _ ->
          error
            ( printf "P3.1: expected locator, but found %s" (latexLine t)
            )

    -- a_n, id with application
    an `App` [l] = last as
    dNow = d sIn

    nextState :: State -> State
    nextState s =
      case bc of
        -- P4.1
        M b free [c] ->
          sEmpty {d = d' + 1, list = [M d0' free [list (rule2 sEmpty {list = c})]]}
        b `ToLocator` c ->
          case stripPrefix pref as of
            -- P4.2
            Nothing ->
              sEmpty {d = d' + 1, list = [d0' `ToLocator` c]}
            -- P4.3
            _ -> rule4 sEmpty {d = d', term = bc, d0 = d0'}
        _ ->
          error
            ( printf
                "R3. Wrong term in locator: %s"
                (latexLine bc)
            )
      where
        bc = term s
        d0' = d0 s
        d' = d s
        pref = takeTillApplication as

    -- base for c-s. allocates names d_1, d_2, ..., d_{n+1}
    dAfter = dNow + length l + 1
    sIn1 = sEmpty {d = dAfter}

    -- list of results of (d_1)|c_1, ...
    sOut1 = foldl combine sIn1 (zip [0 ..] l)
      where
        combine state (id, term) =
          sEmpty {d = d sNext, list = list state ++ list sNext}
          where
            sNext =
              nextState sEmpty {d = d state, d0 = getName (dNow + id), term = term}

    cs = list sOut1

    -- b_1->d_1, b_2->d_2 ...
    l' = [an `App` [zipWith (\bc id -> getB bc `ToLocator` [getD id]) l [0 ..]]]
      where
        getB bc =
          case bc of
            M b _ _ -> b
            b `ToLocator` _ -> b
            _ ->
              error
                ( printf
                    "R3. Wrong term in locator: %s"
                    (latexLine bc)
                )
        getD id =
          getName (dNow + id)

    -- prefix till application with modified application
    as' = init as ++ l'
    -- name for d_{n+1} helper attribute
    d_n_plus_1 = getName (dAfter -1)

    -- for (c, d_{n+1})|E
    sIn2 = sEmpty {d = d sOut1, c = c, d0 = d_n_plus_1}
    sOut2 = rule5 sIn2

    sOut = sEmpty {d = d sOut2, list = (d_n_plus_1 `ToLocator` as') : cs}

getName :: Int -> AttributeName
getName d = A (printf "_%d" d)

rule4 :: State -> State
rule4 = const sEmpty {list = [A "R4: to be implemented"]}

rule5 :: State -> State
rule5 = const sEmpty {list = [A "R5: to be implemented"]}