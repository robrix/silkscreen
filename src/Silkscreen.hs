{-# LANGUAGE TypeFamilies #-}
-- | Silkscreen is a library of pretty-printing transformers built around the @prettyprinter@ package. This module defines the core 'Printer' abstraction and a few instances.
--
-- More documentation can be found in "Prettyprinter".
module Silkscreen
( -- * Primitives
  Printer(..)
  -- * Combinators
, pretty
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
) where

import           Control.Applicative (liftA2)
import qualified Prettyprinter as P

-- | A 'Printer' abstracts pretty-printing to allow the composition of behaviours such as e.g. rainbow parentheses, precedence handling, and so forth.
class Monoid p => Printer p where
  -- | The type of annotations supported by the printer.
  --
  -- We provide this as a type family instead of defining 'Printer' over kind @Type -> Type@ in order to allow instances to constrain annotations.
  type Ann p

  -- | Lift a 'P.Doc' to a 'Printer'.
  fromDoc :: P.Doc (Ann p) -> p

  -- | Annotate a 'Printer' with an @'Ann' p@.
  annotate :: Ann p -> p -> p


  -- | Try to unwrap the argument, if it will fit.
  group :: p -> p

  -- | Print the first argument by default, or the second when an enclosing 'group' flattens it.
  flatAlt :: p -> p -> p


  -- | Indent lines in the argument to the current column.
  align :: p -> p

  -- | @'nest' i p@ changes the indentation level for new lines in @p@ by @i@.
  nest :: Int -> p -> p


  -- | Parenthesize the argument.
  --
  -- Overloadable to support e.g. rainbow parentheses.
  parens :: p -> p
  parens = enclose lparen rparen

  -- | Wrap the argument in brackets.
  --
  -- Overloadable to support e.g. rainbow brackets.
  brackets :: p -> p
  brackets = enclose lbracket rbracket

  -- | Wrap the argument in braces.
  --
  -- Overloadable to support e.g. rainbow braces.
  braces :: p -> p
  braces = enclose lbrace rbrace


-- Non-primitive combinators

-- | Pretty-print a value using the 'P.Pretty' instance for its type.
pretty :: (Printer p, P.Pretty t) => t -> p
pretty = fromDoc . P.pretty


concatWith :: (Monoid p, Foldable t) => (p -> p -> p) -> t p -> p
concatWith (<>) ds
  | null ds   = mempty
  | otherwise = foldr1 (<>) ds

vcat :: Printer p => [p] -> p
vcat = concatWith (surround line')

cat :: Printer p => [p] -> p
cat = group . vcat

vsep :: Printer p => [p] -> p
vsep = concatWith (</>)

sep :: Printer p => [p] -> p
sep = group . vsep


-- | @'enclose' l r x@ wraps @x@ in @l@ and @r@.
enclose :: Printer p => p -> p -> p -> p
enclose l r x = l <> x <> r

encloseSep :: Printer p => p -> p -> p -> [p] -> p
encloseSep l r s ps = enclose l r (group (concatWith (surround (line' <> s)) ps))

list :: Printer p => [p] -> p
list
  = group
  . brackets
  . encloseSep
    (flatAlt space mempty)
    (flatAlt space mempty)
    (comma <> space)

tupled :: Printer p => [p] -> p
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
surround :: Printer p => p -> p -> p -> p
surround x l r = enclose l r x

-- | Separate the arguments with a space.
(<+>) :: Printer p => p -> p -> p
(<+>) = surround space

infixr 6 <+>

-- | Separate the arguments with a line.
(</>) :: Printer p => p -> p -> p
(</>) = surround line

infixr 6 </>


-- Symbols

space :: Printer p => p
space = fromDoc P.space

line :: Printer p => p
line = fromDoc P.line

line' :: Printer p => p
line' = fromDoc P.line'

lparen, rparen :: Printer p => p
lparen = fromDoc P.lparen
rparen = fromDoc P.rparen

lbracket, rbracket :: Printer p => p
lbracket = fromDoc P.lbracket
rbracket = fromDoc P.rbracket

lbrace, rbrace :: Printer p => p
lbrace = fromDoc P.lbrace
rbrace = fromDoc P.rbrace

comma :: Printer p => p
comma = fromDoc P.comma


instance Printer (P.Doc ann) where
  type Ann (P.Doc ann) = ann

  fromDoc = id
  annotate = P.annotate

  group = P.group
  flatAlt = P.flatAlt

  align = P.align
  nest = P.nest

  parens = P.parens
  brackets = P.brackets
  braces = P.braces


instance (Printer a, Printer b, Ann a ~ Ann b) => Printer (a, b) where
  type Ann (a, b) = Ann b

  fromDoc d = (fromDoc d, fromDoc d)
  annotate ann (a, b) = (annotate ann a, annotate ann b)

  group (a, b) = (group a, group b)
  flatAlt (a1, b1) (a2, b2) = (flatAlt a1 a2, flatAlt b1 b2)

  align (a, b) = (align a, align b)
  nest i (a, b) = (nest i a, nest i b)

  parens (a, b) = (parens a, parens b)
  brackets (a, b) = (brackets a, brackets b)
  braces (a, b) = (braces a, braces b)


instance Printer b => Printer (a -> b) where
  type Ann (a -> b) = Ann b

  fromDoc = pure . fromDoc
  annotate = fmap . annotate

  group = fmap group
  flatAlt = liftA2 flatAlt

  align = fmap align
  nest i = fmap (nest i)

  parens = fmap parens
  brackets = fmap brackets
  braces = fmap braces
