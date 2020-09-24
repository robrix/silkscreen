module Silkscreen.Rainbow
( RainbowPrinter(..)
) where

import Silkscreen

class Printer p => RainbowPrinter p where
  incrNesting :: p -> p
