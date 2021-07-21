module TransformationRules where

import Commands (Command (..))
import Data.Function ((&))
import Data.List (intercalate)
import qualified LatexConstants as LC (ksi, lambda, phi, quad, rho, upPhi)
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

-- should preserve focus after returning from recursion
-- should change focus before recursion
rule1 :: State -> State
rule1 s =
  sReturn
  where
    premise = premiseTerm s

    -- deconstruct term according to R1
    -- x (a_1, a_2, ..., a_n) -> [[E]]
    (attributeName, freeAttributes, e) =
      case premise of
        M name free [e] ->
          (name, free, e)
        _ ->
          error "R1: incorrect term syntax"

    -- index of vertex at focus-> State
    -- rule2 terms state =
    v_i = focusedElementIndex s
    -- index of new vertex for x
    v_i_x = vertexCounter s + 1

    -- ADD (v_i_x) BIND(v_i, v_i_x, x)
    gmiV_i_x = [ADD v_i_x, BIND v_i v_i_x attributeName]

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

    sForConclusion =
      s
        {
          -- switch focus to vertex v_i_x
          focusedElementIndex = v_i_x,
          -- add 1 vertex for v_i_x and |list of free attributes| vertices
          vertexCounter = vertexCounter s + 1 + freeLength,
          -- children terms don't need current gmis
          gmis = gmis emptyState,
          -- children terms don't need current latexed derivation tree
          latexedRule = latexedRule emptyState
        }

    sFromConclusion = rule2 e sForConclusion

    -- latexed current rule
    rule =
      printf
        "\\dfrac{ v_%d | %s }{ %s %s } R1 "
        (focusedElementIndex s)
        (latexLine (premiseTerm s))
        (getLatexedGmis gmisCurrent)
        (latexedRule sFromConclusion)
        ++ LC.quad

    -- finally, state to return
    sReturn =
      sFromConclusion
        { latexedRule = rule,
          gmis = gmisCurrent ++ gmis sFromConclusion
        }

-- | can come from rule1 or from application
--
-- deals only with mappings
--
-- accumulates changes from terms in some state
rule2 :: [Term] -> State -> State
rule2 terms state =
  sReturn
  where
    combine s1 term =
      nextState {
        latexedRule = latexedRule s1 ++ latexedRule nextState,
        gmis = gmis s1 ++ gmis nextState
      }
      where
        nextState =
          s1 {premiseTerm = term, latexedRule = latexedRule s1}
            & case term of
              M {} -> rule1
              ToLocator {} -> rule3
              ToLambda {} -> rule7
              _ -> error "R2: strange term"

    combinedState = foldl combine state terms

    latexedConclusion = printf " %s " (latexedRule combinedState) :: String

    sReturn =
      combinedState
        { latexedRule =
            case terms of
              [_] -> latexedConclusion
              _ -> 
                printf " \\dfrac { v_%d | %s } { %s } R2"
                  (focusedElementIndex state)
                  (toStringSequence terms)
                  latexedConclusion
        }


-- in: term, empty gmis, empty latexed rule, counters
-- out: gmis, latexed rule

-- I assume d_i are names of attributes that refer to data via lambdas
-- They should be prefixed with a /, like /1

getLatexedGmis :: (Foldable t, Show a) => t a -> [Char]
getLatexedGmis = concatMap show

rule3 :: State -> State
-- rule3 s = s {latexedRule = " \\text{TODO: R3} " ++ LC.quad}
rule3 s =
  sReturn
  where
    term = premiseTerm s
    (a, x, e) =
      case term of
        a `ToLocator` (x:e) -> (a, x, e)
        _ -> error "R3: empty locator"
    e_i_a = edgeCounter s + 1
    v_i = focusedElementIndex s

    gmisCurrent = [REF e_i_a v_i x a]

    -- for v_i | x
    sForVConclusion =
      s {
        focusedElementIndex = v_i,
        premiseTerm = x,
        gmis = gmis emptyState,
        latexedRule = latexedRule emptyState
      }

    -- from v_i_x
    sFromVConclusion = rule6 sForVConclusion

    sForEConclusion =
      sFromVConclusion {
        focusedElementIndex = e_i_a,
        premiseTerm = premiseTerm emptyState,
        latexedRule = latexedRule emptyState,
        gmis = gmis emptyState,
        edgeCounter = e_i_a
      }

    sFromEConclusion =
      sForEConclusion &
      case term of
        _ `ToLocator` [_, A{}] -> rule4 e
        _ `ToLocator` [_, App{}] -> rule5 e
        _ `ToLocator` [_, A{}, _] -> rule4 e
        _ `ToLocator` [_, App{}, _] -> rule5 e
        _ `ToLocator` [_] -> id
        _ -> error "R3: unknown term inside locator"

    sReturn = sFromEConclusion {
      gmis = gmisCurrent ++ gmis sFromEConclusion,
      latexedRule =
        printf
          " \\dfrac { v_%d | %s } {%s %s %s} R3 "
          (focusedElementIndex s)
          (latexLine term)
          (getLatexedGmis gmisCurrent)
          (latexedRule sFromVConclusion)
          (latexedRule sFromEConclusion)
        ++ LC.quad 
    }



rule4 :: [Term] -> State -> State
rule4 t s = s {latexedRule = " \\text{TODO: R4} " ++ LC.quad}

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
