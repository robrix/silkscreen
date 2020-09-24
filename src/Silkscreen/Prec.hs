{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Prec
( PrecPrinter(..)
) where

import Silkscreen

class Printer p => PrecPrinter p where
  type Level p

  -- | Print informed by the current 'Level'.
  askingPrec :: (Level p -> p) -> p

  -- | Locally change the 'Level' in a printer.
  localPrec :: (Level p -> Level p) -> p -> p
