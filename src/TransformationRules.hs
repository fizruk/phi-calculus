module TransformationRules where

import Commands (Command (..))
import Data.Function ((&))
import qualified LatexConstants as LC (ksi, lambda, phi, quad, rho, upPhi)
import LatexLine (latexLine)
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

rule1 :: State -> State
rule1 s =
  s
    { latexedRule = rule,
      gmis = gmisCurrent ++ gmis sFromConclusion
    }
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

    -- | input state for conclusion
    sForConclusion =
      s
        { -- add 1 vertex for v_i_x and |list of free attributes| vertices
          vertexCounter = vertexCounter s + 1 + freeLength,
          -- switch focus to vertex v_i_x
          focusedElementIndex = v_i_x
        }

    -- | output state from conclusion
    -- choose always rule 2 because inside an object,
    -- there may be only comma-separated values
    sFromConclusion = rule2 e sForConclusion

    -- latexed GMIs produced by this rule
    currentLatexedGMIS = concatMap show gmisCurrent

    -- latexed current rule
    rule =
      printf
        "\\dfrac{ v_%d | %s }{ %s %s } R1 "
        (focusedElementIndex s)
        (latexLine (premiseTerm s))
        currentLatexedGMIS
        (latexedRule sFromConclusion)
      ++ LC.quad

rule2 :: [Term] -> State -> State
rule2 terms state = emptyState {latexedRule = "R2"}

--   case terms of
--     [] ->
-- case l of
--       -- if has several attributes, they are separated by commas, so use R2
--       [] -> error "R1: no attributes in Phi"
--       -- if has only 1 attribute, it can either be a locator or an attribute with body
--       [M {}] -> error "rule 1 called"
--       -- rule1 sIncremented
--       [ToLocator {}] -> error "rule 1 called"
--       -- rule2 sIncremented
--       _ -> rule2 sFromConclusion
-- emptyState {latexedRule = "__R2__"}
