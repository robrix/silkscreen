{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Prec
( PrecPrinter(..)
) where

import Silkscreen

class Printer p => PrecPrinter p where
  -- | The type used to represent precedence levels. This is defined as an associated type so that consumers can use e.g. symbolic representations of their DSL’s precedence levels instead of e.g. unsemantic 'Int's.
  --
  -- This type will usually be 'Ord'ered, but this isn’t strictly required so that other means of determining precedence can be provided.
  type Level p

  -- | Print informed by the current 'Level'.
  askingPrec :: (Level p -> p) -> p

  -- | Locally change the 'Level' in a printer.
  localPrec :: (Level p -> Level p) -> p -> p
