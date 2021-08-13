module PhiGrammar
  ( Term (..),
    Mapping,
    AttributeName,
    Lambda,
    Locator,
    AttributeNameOrCopy,
    VertexId,
    FUNCTION(..),
  )
where

import Data.List (intercalate, intersperse, tails)

-- Phi-Term Grammar

-- | any mapping a > b
type Mapping = Term

-- | attribute name
type AttributeName = Term

-- | lambda
type Lambda = Term

-- | list of free attributes
type FreeAttributes = [AttributeName]

-- | represent object body
--
-- imitate double brackets via nested list
type Value = [[Mapping]]

type AttributeNameOrCopy = Term

-- | list of attribute names (with applications)
type Locator = [AttributeNameOrCopy]

data Term
  = -- | \\s.MCommand
    --
    -- L == Lambda
    L FUNCTION
  | -- | attribute name
    A String
  | -- | App == value is Applied to object
    --
    -- a([b>c, d>[e]])
    AttributeName `App` Value
  | -- | M == Mapping of attribute name with (possibly 0) free attributes to object body
    --
    -- a [] > [d > e]
    --
    -- a [b, c] > [d>e]
    M AttributeName FreeAttributes Value
  | -- | ToLocator == mapping of attribute name To Locator
    --
    -- a > b.c
    AttributeName `ToLocator` Locator
  | -- | ToLambda == attribute name maps To some Lambda expression
    --
    -- a > \s.M
    AttributeName `ToLambda` Lambda

-- | Lambda grammar
type VertexId = Int

type Stack = [VertexId]

type INT = FUNCTION

data FUNCTION
  = INTEGER Integer
  | STRING String
  | GO (Locator, Stack, Locator)