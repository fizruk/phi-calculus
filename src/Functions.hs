-- import Data.Bits (xor)
-- import Text.Printf (printf)

-- type Arg = Expression

-- type Args = [Arg]

-- type Arg1 = Arg

-- type Arg2 = Arg

-- type F = Expression

-- type F1 = F

-- type F2 = F

-- type Condition = Expression

-- type Then = Expression

-- type Else = Expression

-- type Branch = Expression

-- data Expression
--   = MAP F Args
--   | --   FP things
--     INTEGER Integer
--   | Arg1 `PLUS` Arg2
--   | Arg1 `MINUS` Arg2
--   | Arg1 `MULTIPLY_BY` Arg2
--   | SQUARE Arg
--   | MOD Arg1 Arg2
--   | INCREMENT Arg1
--   | -- boolean
--     BOOL Bool
--   | Arg1 `AND` Arg2
--   | Arg1 `OR` Arg2
--   | NOT Arg
--   | --   comparison
--     Arg1 `EQUALS_TO` Arg2
--   | Arg1 `LESS_THAN` Arg2
--   | Arg1 `GREATER_THAN` Arg2
--   | --   string
--     STRING String
--   | CONCATENATE Arg1 Arg2
--   | --   branching
--     IF Condition Then Else
--   | --   if condition satisfied, call function
--     BRANCH Condition Expression
--   | -- consider several cases
--     CASE [Branch]

-- t1 :: Expression
-- t1 =
--   INTEGER 3
--     `PLUS` CASE
--       [ BRANCH
--           (INTEGER 3 `EQUALS_TO` INTEGER 4)
--           (INTEGER 5),
--         BRANCH
--           (INTEGER 5 `EQUALS_TO` INTEGER 5)
--           (INTEGER 3)
--       ]

-- evaluate :: Expression -> Expression
-- evaluate t =
--   case t of
--     INTEGER a -> INTEGER a
--     INTEGER a `PLUS` INTEGER b -> INTEGER (a + b)
--     INTEGER a `MINUS` INTEGER b -> INTEGER (a - b)
--     INTEGER a `MULTIPLY_BY` INTEGER b -> INTEGER (a * b)
--     SQUARE a -> a `MULTIPLY_BY` a
--     INTEGER a `MOD` INTEGER b -> INTEGER (mod a b)
--     INCREMENT a -> a `PLUS` INTEGER 1

--     BOOL a -> BOOL a
--     BOOL a `AND` BOOL b -> BOOL (a && b)
--     BOOL a `OR` BOOL b -> BOOL (a || b)
--     NOT (BOOL a) -> BOOL (not a)
--     a `EQUALS_TO` b -> compareBy (==) a b
--     a `LESS_THAN` b -> compareBy (<) a b
--     a `GREATER_THAN` b -> compareBy (>) a b

-- compareBy :: (Expression -> Expression -> Bool) -> Expression -> Expression -> Expression
-- compareBy operator a b = result
--     where
--       aEvaluated = evaluate a
--       bEvaluated = evaluate b
--       result =
--         case (aEvaluated, bEvaluated) of
--         (INTEGER c, INTEGER d) -> BOOL (operator c d)
--         (STRING c, STRING d) -> BOOL (operator c d)
--         _ -> error (printf "%s: compare invalid values")

-- evaluateBOOL :: Expression -> Expression
-- evaluateBOOL t =
--   case t of

--     _ -> error "BOOL: incompatible expression"

-- evaluateSTRING :: Expression -> Expression
-- evaluateSTRING t =
--   case t of
--     STRING a -> STRING a
--     CONCATENATE (STRING a) (STRING b) -> STRING (a ++ b)
--     _ -> error "STRING: incompatible expression"

-- evaluateBRANCHING :: Expression -> Expression
-- evaluateBRANCHING t =
--   case t of
--       IF (BOOL a) b c -> if a == TRUE then b else c
--       CASE a ->
--           case (find (\(BRANCH b c) -> b == TRUE) a) of
--               Just  a -> a
--               _ -> error "BRANCHING: neither condition satisfied"

data Data = INTEGER Integer | Data `Plus` Data