{-# LANGUAGE TypeFamilies #-}
module Silkscreen
( -- * Primitives
  Printer(..)
  -- * Combinators
, enclose
, surround
, (<+>)
  -- * Symbols
, lparen
, rparen
, lbracket
, rbracket
, lbrace
, rbrace
, space
) where

import qualified Prettyprinter as P

-- | A 'Printer' abstracts pretty-printing to allow the composition of behaviours such as e.g. rainbow parentheses, precedence handling, and so forth.
class Monoid p => Printer p where
  -- | The type of annotations supported by the printer.
  --
  -- We provide this as a type family instead of defining 'Printer' over kind @Type -> Type@ in order to allow instances to constrain annotations.
  type Ann p

  -- | Pretty-print a value using the 'P.Pretty' instance for its type.
  pretty :: P.Pretty t => t -> p


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


-- Symbols

lparen, rparen :: Printer p => p
lparen = pretty '('
rparen = pretty ')'

lbracket, rbracket :: Printer p => p
lbracket = pretty '['
rbracket = pretty ']'

lbrace, rbrace :: Printer p => p
lbrace = pretty '['
rbrace = pretty ']'

space :: Printer p => p
space = pretty ' '


instance Printer (P.Doc ann) where
  type Ann (P.Doc ann) = ann

  pretty = P.pretty

  parens = P.parens
  brackets = P.brackets
  braces = P.braces


instance Printer b => Printer (a -> b) where
  type Ann (a -> b) = Ann b

  pretty = pure . pretty

  parens = fmap parens
  brackets = fmap brackets
  braces = fmap braces
