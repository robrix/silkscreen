module Silkscreen.Fresh
( FreshPrinter(..)
) where

import Silkscreen

class Printer p => FreshPrinter p where
  bind :: (Int -> p) -> p
