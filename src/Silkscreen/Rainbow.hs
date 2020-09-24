module Silkscreen.Rainbow
( RainbowPrinter(..)
) where

import Silkscreen

class Printer p => RainbowPrinter p where
  -- | Increment the nesting level of a printer.
  --
  -- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
  incrNesting :: p -> p
