module Silkscreen.Nesting
( -- * Printing with nesting levels
  NestingPrinter(..)
, incrNesting
, encloseNesting
  -- * Re-exports
, module Silkscreen
) where

import Silkscreen

class Printer p => NestingPrinter p where
  -- | Make a printer informed by the current nesting level.
  askingNesting :: (Int -> p) -> p

  -- | Locally change the nesting level for a printer.
  localNesting :: (Int -> Int) -> p -> p

  -- | Apply the current nesting level to a printer.
  --
  -- Different instances can give different meanings to this, e.g. annotating the argument with the nesting level or some other means of rendering it differently.
  applyNesting :: p -> p

-- | Increment the nesting level of a printer.
--
-- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
incrNesting :: NestingPrinter p => p -> p
incrNesting = localNesting succ

encloseNesting :: NestingPrinter p => p -> p -> p -> p
encloseNesting l r = enclose (applyNesting l) (applyNesting r) . incrNesting


instance NestingPrinter b => NestingPrinter (a -> b) where
  askingNesting f = askingNesting . flip f
  localNesting f p = localNesting f . p
  applyNesting p = applyNesting . p
