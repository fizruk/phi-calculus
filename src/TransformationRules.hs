module TransformationRules where

import Commands (Command (..))
import Data.Function ((&))
import Data.List (intercalate, tails)
import qualified LatexConstants as LC (ksi, phi, quad, rho, upPhi)
import LatexLine (latexLine, toStringLocator, toStringSequence, toStringValue)
import qualified PhiTerms as PT(Term (..), Locator, AttributeName)
import qualified SampleTerms as ST
import Text.Printf (printf)

data State = State
  { focusedElementIndex :: Int,
    premiseTerm :: PT.Term,
    latexedRule :: String,
    -- | increment only
    edgeCounter :: Int,
    -- | increment only
    vertexCounter :: Int,
    -- | decrement only
    dataCounter :: Int,
    gmis :: [Command]
  }

initialState :: PT.Term -> State
initialState t = emptyState {premiseTerm = t, gmis = [ADD 0]}

emptyState :: State
emptyState =
  State
    { focusedElementIndex = 0,
      premiseTerm = PT.A "",
      latexedRule = "",
      edgeCounter = 0,
      vertexCounter = 0,
      dataCounter = -1,
      gmis = []
    }

-- putTerm :: Term -> IO ()
putTerm :: PT.Term -> IO ()
putTerm t =
  writeFile "out.txt"
    (printf "\\begin{array}{c} %s \\\\ \\\\ \\mathtt{GMIs} \\\\ %s \\end{array}"
      (latexedRule runTransformation)
      (getLatexedGmis (gmis runTransformation)))
  where runTransformation = rule1 (initialState t)
-- putTerm t = writeFile "out.txt" (getLatexedGmis (gmis (rule1 (initialState t))))

-- TODO: let getting just GMIs
-- getGMIsAfterTransformation :: PT.Term -> [Command]
-- getGMIsAfterTransformation t = rule1 initialState t
   

getLatexedGmis :: [Command] -> [Char]
-- getLatexedGmis = concatMap show
getLatexedGmis commands = printf " \\begin{array}{l} %s \\end{array}" (intercalate " \\\\ " (map show commands))

-- states:
--  in: appropriate focus, premise term (not always), empty gmis, empty latexed rule, counters
--  out: same focused element, gmis, latexed rule, updated counters

rule1 :: State -> State
rule1 sIn = sOut
  where
    premise = premiseTerm sIn

    -- deconstruct term according to R1
    -- x (a_1, a_2, ..., a_n) -> [[E]]
    (x, freeAttributes, e) =
      case premise of
        PT.M name free [e] ->
          (name, free, e)
        _ ->
          error "R1: incorrect term syntax"

    -- index of vertex at focus-> State
    -- rule2 terms state =
    v_i = focusedElementIndex sIn
    -- index of new vertex for x
    v_i_x = vertexCounter sIn + 1

    -- ADD (v_i_x) BIND(v_i, v_i_x, x)
    gmiV_i_x = [ADD v_i_x, BIND v_i v_i_x x]

    -- \forall j \in [1;n] (ADD (v_i_x_j) BIND (v_i_x, v_i_x_j, a_j)
    freeLength = length freeAttributes
    getGmisForFreeAttributes (name : xs) =
      [ ADD (v_i_x + freeLength - length xs) ,
        BIND v_i_x (v_i_x + freeLength - length xs) name
      ]
        ++ getGmisForFreeAttributes xs
    getGmisForFreeAttributes _ = []

    gmisFree = getGmisForFreeAttributes freeAttributes

    -- GMIs produced by this rule
    gmisCurrent = gmiV_i_x ++ gmisFree

    sForV =
      sIn
        { -- switch focus to vertex v_i_x
          focusedElementIndex = v_i_x,
          -- add 1 vertex for v_i_x and |list of free attributes| vertices
          vertexCounter = vertexCounter sIn + 1 + freeLength,
          gmis = gmis emptyState,
          latexedRule = latexedRule emptyState
        }

    sFromV = rule2 e sForV

    -- latexed current rule
    rule =
      printf
        "\\dfrac{ v_{%d} | %s }{ %s %s } R1 %s"
        (focusedElementIndex sIn)
        (latexLine (premiseTerm sIn))
        (getLatexedGmis gmisCurrent)
        (latexedRule sFromV)
        LC.quad

    -- finally, state to return
    sOut =
      sFromV
        { focusedElementIndex = focusedElementIndex sIn,
          latexedRule = rule,
          gmis = gmisCurrent ++ gmis sFromV
        }

-- | can come from rule1 or from application
--
-- accumulates changes from terms in some state
rule2 :: [PT.Term] -> State -> State
-- rule2 t s = s {latexedRule = " \\text{TODO: R2} " ++ LC.quad}
rule2 terms sIn = sOut
  where
    -- transfer from current state and term into the next state
    combine state term = sCombined
      where
        nextState =
          state {premiseTerm = term, latexedRule = latexedRule emptyState}
            & case term of
              PT.M {} -> rule1
              PT.ToLocator {} -> rule3
              PT.ToLambda {} -> rule6
              _ -> error "R2: strange term"
        sCombined =
          nextState
            { latexedRule = latexedRule state ++ latexedRule nextState,
              gmis = gmis state ++ gmis nextState
            }

    -- combine all terms' states into one
    sCombinedAll = foldl combine sIn terms

    -- output state from the rule
    sOut =
      sCombinedAll
        { latexedRule =
            case terms of
              -- if one element in the list, we don't need R2
              [_] -> latexedConclusion
              _ ->
                printf
                  " \\dfrac { v_{%d} | %s } { %s } R2"
                  (focusedElementIndex sIn)
                  (toStringSequence terms)
                  latexedConclusion
        }
      where
        latexedConclusion =
          printf " %s " (latexedRule sCombinedAll)

-- I assume d_i are names of attributes that refer to data via lambdas
-- They should be prefixed with a /, like /1

elementsOfLocatorId :: PT.Term -> (PT.AttributeName, [Char])
elementsOfLocatorId x = (xName, xAppObjectLatexed)
  where
    (xName, xAppObjectLatexed) =
      case x of
        name@(PT.A a) ->
          (name, "")
        name@(PT.A a) `PT.App` [object] ->
          ( name,
            printf
              " ( %s ) "
              (toStringValue object)
          )
        _ ->
          error "Unknown term in locator"


latexedRemainingLocator :: PT.Locator  -> [Char]
latexedRemainingLocator e =
  case e of
    [] -> ""
    _ -> "." ++ toStringLocator e

rule3 :: State -> State
-- rule3 s = s {latexedRule = " \\text{TODO: R3} " ++ LC.quad}
rule3 sIn = sOut
  where
    term = premiseTerm sIn
    -- a x E from a -> x E

    (a, x, e) =
      case term of
        a `PT.ToLocator` (x : e) -> (a, x, e)
        _ -> error "R3: empty locator"

    e_i_a = edgeCounter sIn + 1
    v_i = focusedElementIndex sIn

    (xName, xAppObjectLatexed) = elementsOfLocatorId x

    -- gmis produced by this rule
    gmisCurrent = [REF e_i_a v_i xName a]

    -- state for v_i | x
    sForV =
      sIn
        { focusedElementIndex = v_i,
          premiseTerm = xName,
          gmis = gmis emptyState,
          latexedRule = latexedRule emptyState
        }

    -- state from v_i | x
    sFromV = sForV {
      -- no use for rule 6. data are in lambdas
      -- TODO: remove call to v_i|x
      latexedRule = latexedRule emptyState
    }

    -- state for e_i_a | E
    sForE =
      sFromV
        { focusedElementIndex = e_i_a,
          edgeCounter = e_i_a,
          premiseTerm = premiseTerm emptyState,
          latexedRule = latexedRule emptyState,
          gmis = gmis emptyState
        }

    -- state from e_i_a | E
    sFromE =
      case x of
        PT.A{} -> rule4 e sForE
        PT.App {} -> rule5 (x:e) sForE
        _ -> error "R3: Unknown element of locator"

    -- output state from the rule
    sOut =
      sFromE
        { focusedElementIndex = focusedElementIndex sIn,
          gmis = gmisCurrent ++ gmis sFromE,
          latexedRule =
            printf
              " \\dfrac { v_{%d} | %s } {%s %s %s} R3 "
              (focusedElementIndex sIn)
              (latexLine term)
              (getLatexedGmis gmisCurrent)
              (latexedRule sFromV)
              (latexedRule sFromE)
              ++ LC.quad
        }

-- no assumptions on start of list

rule4 :: [PT.Term] -> State -> State
-- rule4 t s = s {latexedRule = " \\text{TODO: R4} " ++ LC.quad}
rule4 t sIn = sOut
  where
    x : e = t
    sIfNotEmptyLocator =
      sFromE
        { focusedElementIndex = focusedElementIndex sIn,
          gmis = gmisCurrent,
          latexedRule =
            printf
              " \\dfrac { e_{%d} | .%s %s %s} { %s %s} R4"
              e_i
              (latexLine xName)
              LC.quad
              (xAppObjectLatexed ++ latexedE)
              (getLatexedGmis gmisCurrent)
              (latexedRule sFromE)
        }
      where
        e_i = focusedElementIndex sIn
        v_i_x = vertexCounter sIn + 1
        e_i_x_1 = e_i + 1

        (xName, xAppObjectLatexed) = elementsOfLocatorId x

        latexedE = latexedRemainingLocator e
          -- latexedRemainingLocator e

        gmisCurrent = [DOT e_i xName v_i_x e_i_x_1]

        sForE =
          sIn
            { focusedElementIndex = e_i_x_1,
              vertexCounter = v_i_x,
              edgeCounter = e_i_x_1
            }

        sFromE =
          sForE
            & case x of
              PT.A {} -> rule4 e
              PT.App {} -> rule5 t
              _ -> error "R4: unknown element of locator"
    sOut =
      case t of
        [] -> sIn
        _ -> sIfNotEmptyLocator

rule5 :: [PT.Term] -> State -> State
-- rule5 t s = s {latexedRule = " \\text{TODO: R5} " ++ LC.quad}
rule5 t sIn = sOut
  where
    (_ `PT.App` [terms]) : e = t

    e_i = focusedElementIndex sIn
    v_i_1 = vertexCounter sIn + 1
    e_i_2 = e_i + 1

    -- latexed E1 from (E1) E2
    e1Latexed = toStringValue terms

    -- latexed E2 from (E1) E2
    e2Latexed =
      case e of
        [] -> ""
        _ -> "." ++ toStringLocator e

    gmisCurrent = [COPY e_i v_i_1 e_i_2]

    sForV =
      sIn
        { focusedElementIndex = v_i_1,
          vertexCounter = v_i_1,
          edgeCounter = e_i_2
        }

    sFromV = rule2 terms sForV

    sForE =
      sFromV
        { focusedElementIndex = e_i_2,
          latexedRule = ""
        }

    sFromE = rule4 e sForE

    sOut =
      sFromE
        { focusedElementIndex = focusedElementIndex sIn,
          gmis = gmisCurrent ++ gmis sFromE,
          latexedRule =
            printf
              "\\dfrac { e_{%d} | ( %s ) %s } { %s %s %s } R5"
              e_i
              e1Latexed
              e2Latexed
              (getLatexedGmis gmisCurrent)
              (latexedRule sFromV)
              (latexedRule sFromE)
        }

rule6 :: State -> State
-- rule6 s = s {latexedRule = " \\text{TODO: R6} " ++ LC.quad}

rule6 sIn = sOut
  where
    v_i = focusedElementIndex sIn
    (a, m) =
      case premiseTerm sIn of
        a `PT.ToLambda` m -> (a, m)
        _ -> error "R6: mapping to non-lambda"

    d_i = dataCounter sIn
    gmisCurrent = [ADD d_i, ATOM d_i m, BIND v_i d_i a]
    sOut = sIn {
      dataCounter = d_i - 1,
      gmis = gmisCurrent
    }

-- rule7 :: State -> State
-- rule7 s = s {latexedRule = " \\text{TODO: R7} " ++ LC.quad}
