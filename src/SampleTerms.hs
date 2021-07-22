module SampleTerms where

import LatexLine (latexLine)
import PhiTerms (Term (..))
import qualified LatexConstants as LC(lambda, lambdaS)
import Text.Printf (printf)

getLambda :: Show a => a -> [Term]
getLambda e = [A "#", A (LC.lambda ++ printf "(%s)" (show e) )]

selectLambda :: Term
selectLambda = A (LC.lambda ++ "(E)") `ToLambda` L (LC.lambdaS ++ " select ( E ) ")

-- I suggest we mark data with /
t4 :: Term
t4 =
  M (A "#") [] [[ -- Data
    selectLambda,
    -- Terms
    M (A "obj") [] [[
      M (A "x") [A "b"] [[
        A "a" `ToLocator` [A "^", A "y"]
      ]],

      A "y" `ToLocator` getLambda "Integer 1",
      M (A "z") [] [[
        A "y" `ToLocator` getLambda "Integer 2",
        A "w"
          `ToLocator` [
            A "#",
            A "x" `App` [[
              A "b" `ToLocator` getLambda "Integer 2"
            ]]
           ]
      ]]
    ]],
    M (A "app") [] [[
      A "@"
        `ToLocator` (getLambda "Function stdout(text)" ++ [
          A "sprintf" `App` [[
            A "arg_1" `ToLocator` getLambda "String %s",
            A "arg_2" `ToLocator` [A "#", A "obj", A "z", A "w", A "a"]
          ]]
        ])
    ]]
  ]]
