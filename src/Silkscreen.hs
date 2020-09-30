{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeOperators #-}
-- | Silkscreen is a library of pretty-printing transformers built around the @prettyprinter@ package. This module defines the core 'Printer' abstraction and a few instances.
--
-- More documentation can be found in "Prettyprinter".
module Silkscreen
( -- * Printing
  Printer(..)
  -- * Combinators
, pretty
, annotate
, group
, flatAlt
, align
, nest
, concatWith
, vcat
, cat
, vsep
, sep
, enclose
, encloseSep
, list
, tupled
, surround
, (<+>)
, (</>)
  -- ** Conditional combinators
, parensIf
  -- * Symbols
, space
, line
, line'
, lparen
, rparen
, lbracket
, rbracket
, lbrace
, rbrace
, comma
, colon
  -- * Re-exports
, P.Pretty
) where

import           GHC.Generics ((:*:)(..))
import qualified Prettyprinter as P

-- | A 'Printer' abstracts pretty-printing to allow the composition of behaviours such as e.g. rainbow parentheses, precedence handling, and so forth.
class (forall a . Monoid (p a)) => Printer p where
  -- | Lift a 'P.Doc' to a 'Printer'.
  liftDoc0 :: P.Doc a -> p a

  -- | Lift a unary function on 'P.Doc' to a 'Printer'.
  liftDoc1 :: (P.Doc a -> P.Doc a) -> (p a -> p a)

  -- | Lift a binary function on 'P.Doc' to a 'Printer'.
  liftDoc2 :: (P.Doc a -> P.Doc a -> P.Doc a) -> (p a -> p a -> p a)


  -- | Parenthesize the argument.
  --
  -- Overloadable to support e.g. rainbow parentheses.
  parens :: p a -> p a
  parens = enclose lparen rparen

  -- | Wrap the argument in brackets.
  --
  -- Overloadable to support e.g. rainbow brackets.
  brackets :: p a -> p a
  brackets = enclose lbracket rbracket

  -- | Wrap the argument in braces.
  --
  -- Overloadable to support e.g. rainbow braces.
  braces :: p a -> p a
  braces = enclose lbrace rbrace


-- Non-primitive combinators

-- | Pretty-print a value using the 'P.Pretty' instance for its type.
pretty :: (Printer p, P.Pretty t) => t -> p a
pretty = liftDoc0 . P.pretty


-- | Annotate a 'Printer' with an @'Ann' p@.
annotate :: Printer p => a -> p a -> p a
annotate = liftDoc1 . P.annotate


-- | Try to unwrap the argument, if it will fit.
group :: Printer p => p a -> p a
group = liftDoc1 P.group

-- | Print the first argument by default, or the second when an enclosing 'group' flattens it.
flatAlt :: Printer p => p a -> p a -> p a
flatAlt = liftDoc2 P.flatAlt


-- | Indent lines in the argument to the current column.
align :: Printer p => p a -> p a
align = liftDoc1 P.align

-- | @'nest' i p@ changes the indentation level for new lines in @p@ by @i@.
nest :: Printer p => Int -> p a -> p a
nest = liftDoc1 . P.nest


concatWith :: (Monoid a, Foldable t) => (a -> a -> a) -> t a -> a
concatWith (<>) ds
  | null ds   = mempty
  | otherwise = foldr1 (<>) ds

vcat :: Printer p => [p a] -> p a
vcat = concatWith (surround line')

cat :: Printer p => [p a] -> p a
cat = group . vcat

vsep :: Printer p => [p a] -> p a
vsep = concatWith (</>)

sep :: Printer p => [p a] -> p a
sep = group . vsep


-- | @'enclose' l r x@ wraps @x@ in @l@ and @r@.
enclose :: Printer p => p a -> p a -> p a -> p a
enclose l r x = l <> x <> r

encloseSep :: Printer p => p a -> p a -> p a -> [p a] -> p a
encloseSep l r s ps = enclose l r (group (concatWith (surround (line' <> s)) ps))

list :: Printer p => [p a] -> p a
list
  = group
  . brackets
  . encloseSep
    (flatAlt space mempty)
    (flatAlt space mempty)
    (comma <> space)

tupled :: Printer p => [p a] -> p a
tupled
  = group
  . parens
  . encloseSep
    (flatAlt space mempty)
    (flatAlt space mempty)
    (comma <> space)


-- | @'surround' x l r@ wraps @x@ in @l@ and @r@.
--
-- This is a reordering of 'enclose', but allows for convenient use in e.g. folds:
--
-- >>> 'foldr1' ('surround' ('pretty' ", ")) ['pretty' "apple", 'pretty' "banana"]
-- apple, banana
surround :: Printer p => p a -> p a -> p a -> p a
surround x l r = enclose l r x

-- | Separate the arguments with a space.
(<+>) :: Printer p => p a -> p a -> p a
(<+>) = surround space

infixr 6 <+>

-- | Separate the arguments with a line.
(</>) :: Printer p => p a -> p a -> p a
(</>) = surround line

infixr 6 </>


-- | Conditional parenthesization of a printer. Analogous to 'showParen', but for printers.
parensIf :: Printer p => Bool -> p a -> p a
parensIf True = parens
parensIf _    = id


-- Symbols

space :: Printer p => p a
space = liftDoc0 P.space

line :: Printer p => p a
line = liftDoc0 P.line

line' :: Printer p => p a
line' = liftDoc0 P.line'

lparen, rparen :: Printer p => p a
lparen = liftDoc0 P.lparen
rparen = liftDoc0 P.rparen

lbracket, rbracket :: Printer p => p a
lbracket = liftDoc0 P.lbracket
rbracket = liftDoc0 P.rbracket

lbrace, rbrace :: Printer p => p a
lbrace = liftDoc0 P.lbrace
rbrace = liftDoc0 P.rbrace

comma :: Printer p => p a
comma = liftDoc0 P.comma

colon :: Printer p => p a
colon = liftDoc0 P.colon


instance Printer P.Doc where
  liftDoc0 = id
  liftDoc1 = id
  liftDoc2 = id

  parens = P.parens
  brackets = P.brackets
  braces = P.braces


instance (Printer f, Printer g) => Printer (f :*: g) where
  liftDoc0 d = liftDoc0 d :*: liftDoc0 d
  liftDoc1 f (a :*: b) = liftDoc1 f a :*: liftDoc1 f b
  liftDoc2 f (a1 :*: b1) (a2 :*: b2) = liftDoc2 f a1 a2 :*: liftDoc2 f b1 b2

  parens (a :*: b) = parens a :*: parens b
  brackets (a :*: b) = brackets a :*: brackets b
  braces (a :*: b) = braces a :*: braces b
