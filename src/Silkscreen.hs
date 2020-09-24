{-# LANGUAGE TypeFamilies #-}
module Silkscreen
( -- * Primitives
  Printer(..)
  -- * Combinators
, pretty
, enclose
, surround
, (<+>)
, (</>)
  -- * Symbols
, lparen
, rparen
, lbracket
, rbracket
, lbrace
, rbrace
, space
, line
) where

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


-- | @'enclose' l r x@ wraps @x@ in @l@ and @r@.
enclose :: Printer p => p -> p -> p -> p
enclose l r x = l <> x <> r

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

lparen, rparen :: Printer p => p
lparen = fromDoc P.lparen
rparen = fromDoc P.rparen

lbracket, rbracket :: Printer p => p
lbracket = fromDoc P.lbracket
rbracket = fromDoc P.rbracket

lbrace, rbrace :: Printer p => p
lbrace = fromDoc P.lbrace
rbrace = fromDoc P.rbrace

space :: Printer p => p
space = fromDoc P.space

line :: Printer p => p
line = fromDoc P.line


instance Printer (P.Doc ann) where
  type Ann (P.Doc ann) = ann

  fromDoc = id
  annotate = P.annotate

  group = P.group

  parens = P.parens
  brackets = P.brackets
  braces = P.braces


instance Printer b => Printer (a -> b) where
  type Ann (a -> b) = Ann b

  fromDoc = pure . fromDoc
  annotate = fmap . annotate

  group = fmap group

  parens = fmap parens
  brackets = fmap brackets
  braces = fmap braces
