module Silkscreen.Rainbow
( -- * Printing with nesting levels
  RainbowPrinter(..)
  -- * Rainbow parentheses
, Rainbow(..)
) where

import Silkscreen

class Printer p => RainbowPrinter p where
  -- | Increment the nesting level of a printer.
  --
  -- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
  incrNesting :: p -> p


newtype Rainbow a = Rainbow (Int -> a)
