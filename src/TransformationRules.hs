module TransformationRules where

import Commands (Command (..))
import Data.Function ((&))
import Data.List (intercalate)
import qualified LatexConstants as LC (ksi, phi, quad, rho, upPhi)
import LatexLine (latexLine, toStringSequence)
import PhiTerms (Term (..))
import qualified SampleTerms as ST
import Text.Printf (printf)

data State = State
  { focusedElementIndex :: Int,
    premiseTerm :: Term,
    latexedRule :: String,
    -- | increment only
    edgeCounter :: Int,
    -- | increment only
    vertexCounter :: Int,
    -- | decrement only
    dataCounter :: Int,
    gmis :: [Command]
  }

initialState :: Term -> State
initialState t = emptyState {premiseTerm = t, gmis = [ADD 0]}

emptyState :: State
emptyState =
  State
    { focusedElementIndex = 0,
      premiseTerm = A "",
      latexedRule = "",
      edgeCounter = 0,
      vertexCounter = 0,
      dataCounter = -1,
      gmis = []
    }

putTerm :: Term -> IO ()
putTerm t = putStrLn (latexedRule (rule1 (initialState t)))

-- in: appropriate focus, term, empty gmis, empty latexed rule, counters
-- out: gmis, latexed rule, updated counters

rule1 :: State -> State
rule1 sIn = sOut
  where
    premise = premiseTerm sIn

    -- deconstruct term according to R1
    -- x (a_1, a_2, ..., a_n) -> [[E]]
    (x, freeAttributes, e) =
      case premise of
        M name free [e] ->
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
      [ ADD v_i_x,
        BIND v_i_x (v_i_x + freeLength - length xs) name
      ]
        ++ getGmisForFreeAttributes xs
    getGmisForFreeAttributes _ = []

    gmisFree = getGmisForFreeAttributes freeAttributes

    -- GMIs produced by this rule
    gmisCurrent = gmiV_i_x ++ gmisFree

    sForV =
      sIn
        {
          -- switch focus to vertex v_i_x
          focusedElementIndex = v_i_x,
          -- add 1 vertex for v_i_x and |list of free attributes| vertices
          vertexCounter = vertexCounter sIn + 1 + freeLength,
          -- children terms don't need current gmis
          gmis = gmis emptyState,
          -- children terms don't need current latexed derivation tree
          latexedRule = latexedRule emptyState
        }

    sFromV = rule2 e sForV

    -- latexed current rule
    rule =
      printf
        "\\dfrac{ v_%d | %s }{ %s %s } R1 "
        (focusedElementIndex sIn)
        (latexLine (premiseTerm sIn))
        (getLatexedGmis gmisCurrent)
        (latexedRule sFromV)
        ++ LC.quad

    -- finally, state to return
    sOut =
      sFromV
        { latexedRule = rule,
          gmis = gmisCurrent ++ gmis sFromV
        }

-- | can come from rule1 or from application
--
-- accumulates changes from terms in some state
rule2 :: [Term] -> State -> State
rule2 terms sIn = sOut
  where
    -- transfer from current state and term into the next state
    combine state term = sCombined
      where
        nextState =
          state {premiseTerm = term, latexedRule = latexedRule emptyState}
            & case term of
              M {} -> rule1
              ToLocator {} -> rule3
              ToLambda {} -> rule7
              _ -> error "R2: strange term"
        sCombined = 
          nextState {
            latexedRule = latexedRule state ++ latexedRule nextState,
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
                printf " \\dfrac { v_%d | %s } { %s } R2"
                  (focusedElementIndex sIn)
                  (toStringSequence terms)
                  latexedConclusion
        }
      where 
        latexedConclusion = 
          printf " %s " (latexedRule sCombinedAll)


-- I assume d_i are names of attributes that refer to data via lambdas
-- They should be prefixed with a /, like /1

getLatexedGmis :: (Foldable t, Show a) => t a -> [Char]
getLatexedGmis = concatMap show

rule3 :: State -> State
-- rule3 s = s {latexedRule = " \\text{TODO: R3} " ++ LC.quad}
rule3 s =
  sOut
  where
    term = premiseTerm s
    (a, x, e) =
      case term of
        a `ToLocator` (x:e) -> (a, x, e)
        _ -> error "R3: empty locator"
    e_i_a = edgeCounter s + 1
    v_i = focusedElementIndex s

    gmisCurrent = [REF e_i_a v_i x a]

    -- state for v_i | x
    sForV =
      s {
        focusedElementIndex = v_i,
        premiseTerm = x,
        gmis = gmis emptyState,
        latexedRule = latexedRule emptyState
      }

    -- state from v_i | x
    sFromV = rule6 sForV

    -- state for e_i_a | E
    sForE =
      sFromV {
        focusedElementIndex = e_i_a,
        
        premiseTerm = premiseTerm emptyState,
        latexedRule = latexedRule emptyState,
        gmis = gmis emptyState,

        edgeCounter = e_i_a
      }

    -- state from e_i_a | E
    sFromE =
      sForE &
      case term of
        _ `ToLocator` [_, A{}] -> rule4 e
        _ `ToLocator` [_, App{}] -> rule5 e
        _ `ToLocator` [_, A{}, _] -> rule4 e
        _ `ToLocator` [_, App{}, _] -> rule5 e
        _ `ToLocator` [_] -> id
        _ -> error "R3: unknown term inside locator"

    -- output state from the rule
    sOut = sFromE {
      gmis = gmisCurrent ++ gmis sFromE,
      latexedRule =
        printf
          " \\dfrac { v_%d | %s } {%s %s %s} R3 "
          (focusedElementIndex s)
          (latexLine term)
          (getLatexedGmis gmisCurrent)
          (latexedRule sFromV)
          (latexedRule sFromE)
        ++ LC.quad 
    }



rule4 :: [Term] -> State -> State
rule4 t s = s {latexedRule = " \\text{TODO: R4} " ++ LC.quad}
-- rule4 t s = sReturn
--   where
    
--     sReturn = s



rule5 :: [Term] -> State -> State
rule5 t s = s {latexedRule = " \\text{TODO: R5} " ++ LC.quad}

rule6 :: State -> State
rule6 s = s {latexedRule = " \\text{TODO: R6} " ++ LC.quad}
-- rule6 s = 
--   case x of
--         -- if data
--         A ('/':_) -> sReturn
--         -- else
--         _ -> s {
--           latexedRule = 
--             printf 
--               " \\ dfrac {v_%d | %s} {} "
--               v_i
--               (latexLine x)
--         }
--   where 
--     x = premiseTerm s

rule7 :: State -> State
rule7 s = s {latexedRule = " \\text{TODO: R7} " ++ LC.quad}
