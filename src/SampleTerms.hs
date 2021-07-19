module SampleTerms where

import PhiTerms (Term(..), linePrint)

t1 :: Term
t1 =
  Abstract (A"a") [A"b", A"c"] [[
    A"d" `ToLocator` [A"^"]
  ]]

t2 :: Term
t2 =
  Closed (A"obj") [[
    Abstract (A"x") [A"b"] [[
      A"a" `ToLocator` [A"^",A"y"]
    ]]
  ]]

t4 :: Term
t4 =
  Closed (A"#") [[
    -- Data
    A"1" `ToLambda` L"\\s.(Int\\ 1)",
    A"2" `ToLambda` L"\\s.(Int\\ 2)",

    -- Terms
    Closed (A"obj") [[
      Abstract (A"x") [A"b"] [[
        A"a" `ToLocator` [A"^",A"y"]
      ]],
      A"y" `ToLocator` [A"1"],

      Closed (A"z") [[
        A"y" `ToLocator` [A"#", A"2"],
        A"w" 
          `ToLocator` [
            A"#", 
            A"x" `App` [[
              A"b" 
              `ToLocator` [
                A"#", A"2"
              ]
            ]]
          ]
      ]]
    ]],

    A"stdout" `ToLambda` L"\\s.(Function\\ stdout)",

    Closed (A"app") [[
      A"@" 
        `ToLocator` [
          A"#", 
          A"stdout", 
          A"sprintf" `App` [[
            A"arg_1" `ToLocator` [A"#", A"\"\\%s\""],
            A"arg_2" `ToLocator` [A"#", A"obj", A"z", A"w", A"a"]
          ]]
        ]
    ]]
  ]]

