module SampleTerms where

import LatexLine (latexLine)
import PhiTerms (Term (..))
import qualified LatexConstants as LC(lambda, lambdaS)
import Text.Printf (printf)

getLambda :: String -> [Term]
getLambda e = [A "#", A (LC.lambda ++ printf " ( %s ) " e )]

lambdaSelect :: Term
lambdaSelect = A (LC.lambda ++ "(E)") `ToLambda` L " select ( E ) "


-- how to distinguish between data and non-data?
-- possibly, data is always \lambda (Expr)

-- what if a vertex is a locator to data?
-- then during dataization, we'll come to the actual vertex

t4 :: Term
t4 =
  M (A "#") [] [[ -- Data
    lambdaSelect,
    -- Terms
    M (A "obj") [] [[
      M (A "x") [A "b"] [[
        A "a" `ToLocator` [A "^", A "y"]
      ]],

      A "y" `ToLocator` getLambda "Integer\\ 1",
      M (A "z") [] [[
        A "y" `ToLocator` getLambda "Integer\\ 2",
        A "w"
          `ToLocator` [
            A "#",
            A "x" `App` [[
              A "b" `ToLocator` getLambda "Integer\\ 2"
            ]]
           ]
      ]]
    ]],
    M (A "app") [] [[
      A "@"
        `ToLocator` (getLambda "Function\\ stdout(text)" ++ [
          A "sprintf" `App` [[
            A "arg_1" `ToLocator` getLambda "String\\ \"\\%s\"",
            A "arg_2" `ToLocator` [A "#", A "obj", A "z", A "w", A "a"]
          ]]
        ])
    ]]
  ]]
