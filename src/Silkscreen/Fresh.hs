module Silkscreen.Fresh
( FreshPrinter(..)
  -- * Re-exports
, module Silkscreen
) where

import Silkscreen

class Printer p => FreshPrinter p where
  bind :: (Int -> p) -> p


instance FreshPrinter b => FreshPrinter (a -> b) where
  bind f = bind . flip f
