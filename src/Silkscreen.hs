{-# LANGUAGE TypeFamilies #-}
module Silkscreen
( Printer(..)
) where

import qualified Prettyprinter as P

-- | A 'Printer' abstracts pretty-printing to allow the composition of behaviours such as e.g. rainbow parentheses, precedence handling, and so forth.
class Printer p where
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

  -- | Wrap the argument in brackets.
  --
  -- Overloadable to support e.g. rainbow brackets.
  brackets :: p -> p

  -- | Wrap the argument in braces.
  --
  -- Overloadable to support e.g. rainbow braces.
  braces :: p -> p


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
