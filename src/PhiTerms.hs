module PhiTerms
  ( Term (..),
    Mapping,
    AttributeName,
    Locator,
  )
where

import Data.List (intercalate, intersperse, tails)

-- Term Grammar
-- | any mapping a > b
type Mapping = Term
-- | attribute name
type AttributeName = Term
-- | lambda
type L = Term
-- | list of free attributes
type FreeAttributes = [AttributeName]
-- | represent object body
-- 
-- imitate double brackets via nested list
type Value = [[Mapping]]

type AttributeNameOrCopy = Term
-- | list of attribute names and applications
type Locator = [AttributeNameOrCopy]


data Term
  = -- | \\s.M
    --
    -- L == Lambda
    L String
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
  | -- | ToLocator == attribute name maps To Locator
    --
    -- a > b.c
    AttributeName `ToLocator` Locator
  | -- | ToLambda == attribute name maps To some Lambda expression
    --
    -- a > \s.M
    AttributeName `ToLambda` L