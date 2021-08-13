module PreprocessingRules where

import PhiTerms

-- preprocess::Term -> Term
-- preprocess t =

data State = State
  { term :: Term,
    list :: [Term],
    attributeName :: AttributeName,
    d :: Int
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
            addFree (f : fs) = M f [] [[]] : addFree fs
            addFree [] = []
            sIn1 = sIn {list = e}
            sOut1 = rule2 sIn1
        _ -> error "P1: wrong term"

rule2 :: State -> State
rule2 sIn = sOut
  where
    es = list sIn
    chooseRule x =
      case x of
        M {} -> rule1 sIn {term = x}
        ToLocator {} -> rule3 sIn {term = x}
        _ -> error "P2: wrong term"
    sOut

rule3 :: State -> State
rule3 x = x