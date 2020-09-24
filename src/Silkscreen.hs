{-# LANGUAGE TypeFamilies #-}
module Silkscreen
( Printer(..)
) where

import qualified Prettyprinter as P

class Printer p where
  type Ann p

  -- | Parenthesize the argument.
  --
  -- Overloadable to support e.g. rainbow parentheses.
  parens :: p -> p


instance Printer (P.Doc ann) where
  type Ann (P.Doc ann) = ann

  parens = P.parens
