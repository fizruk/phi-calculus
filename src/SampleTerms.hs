module SampleTerms where

import qualified LatexConstants as LC (lambda, lambdaS)
import LatexLine (latexLine)
import PhiGrammar (Term (..))
import Text.Printf (printf)

getLambda :: String -> [Term]
getLambda e = [A "#", A LC.lambda `App` [[A "expr" `ToLambda` L e]]]

specialLambdaAttribute :: Term
specialLambdaAttribute = A (LC.lambda ++ "( expr )") `ToLambda` L " select ( expr ) "

-- how to distinguish between data and non-data?
-- possibly, data is always \lambda (Expr)

-- what if a vertex is a locator to data?
-- then during dataization, we'll come to the actual vertex

t3 :: Term
t3 =
  M (A"#") [] [[
    M (A"x") [A"c"] [[]],
    A"e"
      `ToLocator` [
          A"$",
          A"x"
            `App`[[
              M (A"c") [A"d"] [[]]
            ]],
          A"c"
            `App` [[
              A"d" `ToLambda` L "3"
            ]]
      ]
  ]]


t4 :: Term
t4 =
  M (A "#") [] [[ -- Data
    specialLambdaAttribute,
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

t5 :: Term
t5 =
  M (A"#") [] [[
    A"x" `ToLambda` L "Integer\\ 3",
    A"y" `ToLambda` L "Integer\\ 4",
    A"sum(x, y)" `ToLambda` L "Function\\ sum(\\ksi.x, \\ksi.y)"
  ]]

t6 :: Term
t6 =
  M (A"#") [] [[
    M (A"a") [A"x"] [[
      A"y" `ToLocator` [A"$", A"x"]
    ]],
    A"a1"
      `ToLocator` [
        A"a" `App` [[A"x" `ToLambda` L "Integer\\ 3"]]
      ]
  ]]

t7 :: Term
t7 =
  M (A"#") [] [[
    M (A"a") [A"x"] [[
      A"y" `ToLocator` [A"$", A"$", A"x"]
    ]],
    A"a1"
      `ToLocator` [
        A"a" `App` [[A"x" `ToLambda` L "Integer\\ 3"]]
      ]
  ]]

t8 :: Term
t8 =
  M (A"#") [] [[
    M (A"a") [A"x"] [[
      M (A"y") [A"z"] [[A"w" `ToLocator` [A"^", A"x"]]]
    ]],
    A"b" `ToLocator` [A"$",A"a" `App` [[A"x" `ToLambda` L "1"]]],
    A"c" `ToLocator` [A"$",A"b",A"y" `App` [[A"z" `ToLambda` L "3"]]]
  ]]