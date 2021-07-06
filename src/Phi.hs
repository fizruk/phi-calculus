{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Phi where

import           Data.Hashable             (Hashable)
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.String               (IsString (..))
import           GHC.Generics              (Generic)

import           Data.Text.Prettyprint.Doc (Doc, Pretty (..), (<+>))
import qualified Data.Text.Prettyprint.Doc as Doc

-- | Identifier (for variables and normal attributes).
type Ident = String

-- | Attributes.
data Attr
  = AttrIdent Ident   -- ^ Normal attribute: \(t.a\).
  | AttrPhi           -- ^ Decorator attribute: \(t.\varphi\).
  | AttrDelta         -- ^ Data attribute: \(t.\delta\).
  | AttrRho           -- ^ Parent attribute: \(t.\rho\).
  | AttrXi            -- ^ Current object attribute: \(t.\xi\).
  deriving (Eq, Generic, Hashable)

instance Show Attr where show = show . pretty

instance Pretty Attr where pretty = ppAttr

-- FIXME: parse non-normal attributes
instance IsString Attr where
  fromString = AttrIdent . fromString

-- | An object term is just a collection of attribute-term pairs
-- where attributes are unique.
type Object d = HashMap Attr (Term d)

-- | A term of \(\varphi\)-calculus.
data Term d
  = Var Attr
  -- ^ Variable is a locator for some attribute: \(x\).
  | App (Term d) (Ident, Term d)
  -- ^ Application: \(t(a \mapsto s)\).
  | Object (Object d)
  -- ^ Object: \([\![ a_1 \mapsto t_1, \ldots, a_n \mapsto t_n ]\!]\).
  | Dot (Term d) Attr
  -- ^ Dot notation: \(t.a\).
  | FreeAttr
  -- ^ Marker for a free attribute: \(\varnothing\).
  | Data d
  -- ^ Data (for final values).
  | Atom Ident Int [Object d] ([Object d] -> Object d)
  -- ^ Atom (for built-in primitives).

-- | Perform substitution in a \(\varphi\)-term.
--
-- The most important part here is that \(\xi\) attributes convert to \(\rho\),
-- \(\rho\) disappears and a new \(\xi\) attribute is added
-- when going recursively into an object.
substitute :: Object d -> Term d -> Term d
substitute substs = \case
  Var x ->
    case HashMap.lookup x substs of
      Just s  -> s
      Nothing -> Var x
  Dot t a ->
    Dot (substitute substs t) a
  App t (a, s) ->
    App (substitute substs t) (a, substitute substs s)
  Object o ->
    let shadowed = AttrRho : HashMap.keys o
        xiToRho =
          case HashMap.lookup AttrXi substs of
            Just parent -> HashMap.insert AttrRho parent . HashMap.delete AttrXi
            Nothing -> id
        update = xiToRho . HashMap.filterWithKey (\k _ -> k `notElem` shadowed)
     in Object (HashMap.map (substitute (update substs)) o)
  t@FreeAttr{} -> t
  t@Data{} -> t
  t@Atom{} -> t

-- | Reduce a term to WHNF (weak head normal form).
whnf :: Term d -> Term d
whnf = whnfWith []

-- | Reduce a term to WHNF (weak head normal form) given current stack of ancestor objects.
whnfWith :: [Object d] -> Term d -> Term d
whnfWith parents = \case
  App u (a, v) ->
    case whnfWith parents u of
      Object u' ->
        case AttrIdent a `inObject` u' of
          Just FreeAttr -> Object (u' `with` [(AttrIdent a, v)])
          Nothing       -> error ("attribute " <> show a <> " is missing!")
          Just _        -> error ("attribute " <> show a <> " is not free!")
      u' -> App u' (a, v)

  Object o -> Object o

  Dot u a ->
    case whnfWith parents u of
      u'@(Object o) ->
        case a `inObject` o of
          Just v -> whnfWith (o:parents) (substitute (o `with` [(AttrXi, u')]) (addParentToAtoms o v))
          Nothing ->
            case AttrPhi `inObject` o of
              Just v -> whnfWith (o:parents) (Dot (substitute (o `with` [(AttrXi, u')]) (addParentToAtoms o v)) a)
              Nothing -> error ("attribute " <> show a <> " not found in an object")
      u' -> Dot u' a

  t@FreeAttr{} -> t
  t@Data{} -> t
  t@Var{} -> t
  Atom _name lvl outers atom ->
    -- FIXME: make atoms less hacky
    -- Here we take only lvl parents since later parents have been fixed for this atom as outers.
    Object (atom (take lvl parents <> outers))

-- | Instantiate one of the ancestors of each atom in the given term.
addParentToAtoms :: Object d -> Term d -> Term d
addParentToAtoms o = go 0
  where
    go i = \case
      App t (a, s) -> App (go i t) (a, go i s)
      Object obj -> Object (go (i + 1) <$> obj)
      Dot t a -> Dot (go i t) a
      t@FreeAttr{} -> t
      t@Data{} -> t
      t@Var{} -> t
      Atom name lvl outers atom
        | i == lvl - 1 || lvl == 999 -> Atom name i (o:outers) atom
        | i >= lvl     -> Atom name lvl outers atom
        | otherwise    -> error ("impossible: i=" <> show i <> " lvl=" <> show lvl)

-- ** Dataization

dataize :: Term d -> Either (Term d) d
dataize = dataizeWith []

dataize' :: Pretty d => Term d -> d
dataize' = dataizeWith' []

dataizeWith :: [Object d] -> Term d -> Either (Term d) d
dataizeWith parents t =
  case whnfWith parents (Dot t AttrDelta) of
    Data d -> Right d
    s      -> Left s

dataizeWith' :: [Object d] -> Term d -> d
dataizeWith' parents t =
  case dataizeWith parents t of
    Right d -> d
    Left _s -> error ("non-data value found for attribute " <> show (ppAttr AttrDelta))

-- * Pretty-printing

ppTerm :: Pretty d => Term d -> Doc ann
ppTerm = \case
  Var a -> ppAttr a
  App t (a, s) -> ppTerm t <> Doc.parens (Doc.hang 2 $ Doc.pretty a <+> "↦" <+> ppTerm s)
  Dot t a -> ppLocator t <> Doc.softline' <> "." <> ppAttr a
  Object o -> Doc.align $ Doc.encloseSep "⟦ " " ⟧" (Doc.comma <+> "")
    (ppAttrWithValue <$> HashMap.toList o)
  FreeAttr -> "∅"
  Data d -> pretty d
  Atom name _lvl _outers _atom -> "<ATOM " <> pretty name <> ">"

ppAttrWithValue :: Pretty d => (Attr, Term d) -> Doc ann
ppAttrWithValue (a, t) = ppAttr a <+> "↦" <+> Doc.align (ppTerm t)

ppLocator :: Pretty d => Term d -> Doc ann
ppLocator = \case
  t@App{} -> Doc.parens (ppTerm t)
  t       -> ppTerm t

ppAttr :: Attr -> Doc ann
ppAttr = \case
  AttrIdent x -> Doc.pretty x
  AttrPhi     -> "𝜑"
  AttrDelta   -> "δ"
  AttrRho     -> "ρ"
  AttrXi      -> "ξ"

instance Pretty d => Show (Term d) where show = show . pretty
instance Pretty d => Pretty (Term d) where pretty = ppTerm

-- * Helper functions

-- ** Helpers for 'Object's

with :: Object d -> [(Attr, Term d)] -> Object d
o `with` attrs = HashMap.fromList attrs `HashMap.union` o

inObject :: Attr -> Object d -> Maybe (Term d)
inObject = HashMap.lookup

mkData :: d -> Term d
mkData d = Object $ HashMap.fromList
  [ (AttrDelta, Data d) ]

-- ** Helpers for definition of atoms

mkAtom0 :: (d -> d) -> [Object d] -> Object d
mkAtom0 f parents@(_:rho:_) = rho `with` [(AttrDelta, Data (f x))]
  where
    x = dataizeWith' parents (Object rho)
mkAtom0 _ _ = error "atom used outside of a proper parent object"

mkAtom1 :: (d -> d -> d) -> [Object d] -> Object d
mkAtom1 f parents@(xi:rho:_) = rho `with` [(AttrDelta, Data (f x y))]
  where
    x = dataizeWith' parents (Object rho)
    y = dataizeWith' parents (Dot (Object xi) (AttrIdent "_1"))
mkAtom1 _ _ = error "atom used outside of a proper parent object"

mkAtom2 :: (d -> d -> d -> d) -> [Object d] -> Object d
mkAtom2 f parents@(xi:rho:_) = rho `with` [(AttrDelta, Data (f x y z))]
  where
    x = dataizeWith' parents (Object rho)
    y = dataizeWith' parents (Dot (Object xi) (AttrIdent "_1"))
    z = dataizeWith' parents (Dot (Object xi) (AttrIdent "_2"))
mkAtom2 _ _ = error "atom used outside of a proper parent object"

-- * Atoms for 'Int' data

-- ** Atoms for numbers

atom_num_inc :: Num d => [Object d] -> Object d
atom_num_inc = mkAtom0 (+1)

atom_num_add :: Num d => [Object d] -> Object d
atom_num_add = mkAtom1 (+)

atom_num_sub :: Num d => [Object d] -> Object d
atom_num_sub = mkAtom1 (-)

atom_num_mul :: Num d => [Object d] -> Object d
atom_num_mul = mkAtom1 (*)

atom_num_less :: (Ord d, Num d) => [Object d] -> Object d
atom_num_less parents@(xi:rho:_) = b
  where
    Object b = mkBool (x < y)
    x = dataizeWith' parents (Object rho)
    y = dataizeWith' parents (Dot (Object xi) (AttrIdent "_1"))
atom_num_less _ = error "atom used outside of a proper parent object"

-- ** Atoms for floating point arithmetic

atom_float_sqrt :: Floating d => [Object d] -> Object d
atom_float_sqrt = mkAtom0 sqrt

-- ** Atoms for booleans

atom_bool_not :: (Eq d, Num d) => [Object d] -> Object d
atom_bool_not = mkAtom0 (\x -> 1 - x)

atom_bool_if :: (Eq d, Num d) => [Object d] -> Object d
atom_bool_if = mkAtom2 (\b x y -> if b /= 0 then x else y)

-- ** Integer and boolean smart constructors

-- |
-- >>> mkNum 3
-- ⟦ mul ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_mul> ⟧
-- , sub ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_sub> ⟧
-- , less ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_less> ⟧
-- , δ ↦ 3
-- , inc ↦ ⟦ 𝜑 ↦ <ATOM num_inc> ⟧
-- , add ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_add> ⟧ ⟧
mkNum :: (Ord d, Num d) => d -> Term d
mkNum n = Object $ HashMap.fromList
  [ (AttrDelta, Data n)
  , (AttrIdent "inc", (Object (HashMap.fromList [(AttrPhi, Atom "num_inc" 999 [] atom_num_inc)])))
  , (AttrIdent "add", (Object (HashMap.fromList [(AttrIdent "_1", FreeAttr), (AttrPhi, Atom "num_add" 999 [] atom_num_add)])))
  , (AttrIdent "sub", (Object (HashMap.fromList [(AttrIdent "_1", FreeAttr), (AttrPhi, Atom "num_sub" 999 [] atom_num_sub)])))
  , (AttrIdent "mul", (Object (HashMap.fromList [(AttrIdent "_1", FreeAttr), (AttrPhi, Atom "num_mul" 999 [] atom_num_mul)])))
  , (AttrIdent "less", (Object (HashMap.fromList [(AttrIdent "_1", FreeAttr), (AttrPhi, Atom "num_less" 999 [] atom_num_less)])))
  ]

mkFloating :: (Ord d, Floating d) => d -> Term d
mkFloating x = Object $ o `with`
  [ ("sqrt", (Object (HashMap.fromList [(AttrPhi, Atom "float_sqrt" 999 [] atom_float_sqrt)])))
  ]
  where
    Object o = mkNum x

-- |
-- >>> mkInt 3
-- ⟦ mul ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_mul> ⟧
-- , sub ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_sub> ⟧
-- , less ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_less> ⟧
-- , δ ↦ 3
-- , inc ↦ ⟦ 𝜑 ↦ <ATOM num_inc> ⟧
-- , add ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM num_add> ⟧ ⟧
mkInt :: Int -> Term Int
mkInt = mkNum

mkDouble :: Double -> Term Double
mkDouble = mkFloating

-- |
-- >>> mkBool False
-- ⟦ if ↦ ⟦ _1 ↦ ∅, 𝜑 ↦ <ATOM bool_if>, _2 ↦ ∅ ⟧
-- , not ↦ ⟦ 𝜑 ↦ <ATOM bool_not> ⟧
-- , δ ↦ 0 ⟧
mkBool :: (Eq d, Num d) => Bool -> Term d
mkBool b = Object $ HashMap.fromList
  [ (AttrDelta, Data (if b then 1 else 0))
  , (AttrIdent "if", (Object (HashMap.fromList [(AttrIdent "_1", FreeAttr), (AttrIdent "_2", FreeAttr), (AttrPhi, Atom "bool_if" 999 [] atom_bool_if)])))
  , (AttrIdent "not", (Object (HashMap.fromList [(AttrPhi, Atom "bool_not" 999 [] atom_bool_not)])))
  ]

-- * Examples

-- |
-- >>> ex1
-- ⟦x ↦ ∅,y ↦ x⟧
ex1 :: Term ()
ex1 = Object (HashMap.fromList [(AttrIdent "x", FreeAttr), (AttrIdent "y", Var (AttrIdent "x"))])

-- |
-- >>> ex2
-- ⟦x ↦ ⟦y ↦ ρ⟧⟧
ex2 :: Term ()
ex2 = Object (HashMap.fromList [(AttrIdent "x", Object (HashMap.fromList [(AttrIdent "y", Var AttrRho)]))])

-- |
-- >>> ex3
-- ⟦𝜑 ↦ ⟦x ↦ ⟦⟧,y ↦ x⟧⟧
--
-- >>> whnf $ Dot ex3 (AttrIdent "x")
-- ⟦⟧
-- >>> whnf $ Dot ex3 (AttrIdent "y")
-- ⟦⟧
-- >>> whnf $ Dot ex3 AttrPhi
-- ⟦x ↦ ⟦⟧,y ↦ x⟧
ex3 :: Term ()
ex3 = Object (HashMap.fromList [(AttrPhi, Object (HashMap.fromList [(AttrIdent "x", Object HashMap.empty), (AttrIdent "y", Var (AttrIdent "x"))]))])

ex5 :: Term Int
ex5 = Object $ HashMap.fromList
  [ ("x", mkInt 2)
  , ("y", mkInt 3)
  , ("x_less_y", (App (Dot (Var "x") "less") ("_1", Var "y")))
  , ("x_less_y_if", App (Dot (App (Dot (Var "x") "less") ("_1", Var "y")) "if") ("_1", Var "x"))
  , ("distance_from_x_to_y",
      App (App
        (Dot (App (Dot (Var "x") "less") ("_1", Var "y")) "if")
          ("_1", App (Dot (Var "y") "sub") ("_1", Var "x")))
          ("_2", App (Dot (Var "x") "sub") ("_1", Var "y")))
  ]

ex5' :: Term Int
ex5' = Object $ HashMap.fromList
  [ ("x", mkInt 2)
  , ("y", mkInt 3)
  , ("x_less_y_if", App (Dot (App (Dot (Var "x") "less") ("_1", Var "y")) "if") ("_1", Var "x"))
  ]

ex5'' :: Term Int
ex5'' = Object $ HashMap.fromList
  [ ("x", mkBool False)
  , ("z", App (Dot (Dot (Var "x") "not") "if") ("_1", Var "x"))
  ]

ex5''' :: Term Int
ex5''' = Object $ HashMap.fromList
  [ ("x", mkBool False)
  , ("z", Dot (Dot (Var "x") "not") "not")
  ]

ex5'''' :: Term Int
ex5'''' = Object $ HashMap.fromList
  [ ("z", Dot (Dot (mkBool False) "not") "not")
  ]

ex6 :: Term Int
ex6 = Object $ HashMap.fromList
  [ ("x", Object $ HashMap.fromList [ ("y", mkBool True) ])
  , ("z", App (Dot (Dot (Var "x") "y") "if") ("_1", Dot (Var "x") "y"))
  ]

ex6' :: Term Int
ex6' = Object $ HashMap.fromList
  [ ("x", Object $ HashMap.fromList [ ("y", mkInt 0) ])
  , ("z", Dot (Dot (Var "x") "y") "inc")
  ]


ex7 :: Term Int
ex7 = Object $ HashMap.fromList
  [ ("z", App (Dot (mkBool True) "if") ("_1", Var "x"))
  , ("x", mkBool False)
  ]

ex8 :: Term Int
ex8 = Object $ HashMap.fromList
  [ (AttrPhi, Object $ HashMap.fromList
      [ ("x", mkNum 1)
      , ("y", Dot (Var "x") "inc")
      , (AttrPhi, Var "y")
      ]
    )
  ]

ex9 :: Term d
ex9 = Object $ HashMap.fromList
  [ (AttrPhi, Object $ HashMap.fromList
      [ ("x", Object HashMap.empty)
      , ("y", Var "x")
      , (AttrPhi, Var "y")
      ]
    )
  ]

-- ** Examples from the paper

-- |
-- >>> ex_book :: Term ()
-- ⟦ isbn ↦ ∅ ⟧
ex_book :: Term a
ex_book = Object (HashMap.fromList [(AttrIdent "isbn", FreeAttr)])

-- ex_book2 :: Term (Either (IORef Double) String)
-- ex_book2 = Object $ HashMap.fromList
--   [ (AttrIdent "isbn", FreeAttr)
--   , (AttrIdent "title", mkData (Right "Object thinking")) ]

ex_point :: Term a
ex_point = Object $ HashMap.fromList
  [ ("x", FreeAttr)
  , ("y", FreeAttr)
  , (AttrIdent "distance_to", Object $ HashMap.fromList
      [ ("p", FreeAttr)
      , (AttrPhi, Object $ HashMap.fromList
          [ ("x_sub_p_x", App (Dot (Var "x") "sub") ("_1", Dot (Var "p") "x"))
          , ("y_sub_p_y", App (Dot (Var "y") "sub") ("_1", Dot (Var "p") "y"))
          , ("x2", App (Dot (Var "x_sub_p_x") "mul") ("_1", Var "x_sub_p_x"))
          , ("y2", App (Dot (Var "y_sub_p_y") "mul") ("_1", Var "y_sub_p_y"))
          , (AttrPhi, Dot (App (Dot (Var "x2") "add") ("_1", Var "y2")) "sqrt")
          ])
      ])
  ]

ex_with_point :: Term Double
ex_with_point = App (Dot point_0_0 "distance_to") ("p", point_3_4)
  where
    point_0_0 = App (App ex_point ("x", mkDouble 0)) ("y", mkDouble 0)
    point_3_4 = App (App ex_point ("x", mkDouble 3)) ("y", mkDouble 4)

