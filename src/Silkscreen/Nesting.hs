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
  askingNesting :: (Int -> p a) -> p a

  -- | Locally change the nesting level for a printer.
  localNesting :: (Int -> Int) -> p a -> p a

  -- | Apply the current nesting level to a printer.
  --
  -- Different instances can give different meanings to this, e.g. annotating the argument with the nesting level or some other means of rendering it differently.
  applyNesting :: p a -> p a

-- | Increment the nesting level of a printer.
--
-- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
incrNesting :: NestingPrinter p => p a -> p a
incrNesting = localNesting succ

encloseNesting :: NestingPrinter p => p a -> p a -> p a -> p a
encloseNesting l r = enclose (applyNesting l) (applyNesting r) . incrNesting
