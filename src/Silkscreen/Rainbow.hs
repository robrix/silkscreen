module Silkscreen.Rainbow
( -- * Printing with nesting levels
  RainbowPrinter(..)
  -- * Rainbow parentheses
, runRainbow
, Rainbow(..)
) where

import Silkscreen

class Printer p => RainbowPrinter p where
  -- | Increment the nesting level of a printer.
  --
  -- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
  incrNesting :: p -> p


runRainbow :: Int -> Rainbow a -> a
runRainbow n (Rainbow run) = run n

newtype Rainbow a = Rainbow (Int -> a)
