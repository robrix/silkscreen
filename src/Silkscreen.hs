{-# LANGUAGE TypeFamilies #-}
module Silkscreen
( Printer(..)
) where

import qualified Prettyprinter as P

class Printer p where
  -- | The type of annotations supported by the printer.
  --
  -- We provide this as a type family instead of defining 'Printer' over kind @Type -> Type@ in order to allow instances of Printer to constrain annotations.
  type Ann p

  -- | Parenthesize the argument.
  --
  -- Overloadable to support e.g. rainbow parentheses.
  parens :: p -> p


instance Printer (P.Doc ann) where
  type Ann (P.Doc ann) = ann

  parens = P.parens
