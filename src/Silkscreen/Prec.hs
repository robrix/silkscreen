{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Prec
( PrecPrinter(..)
) where

import Silkscreen

class Printer p => PrecPrinter p where
  type Level p

  askingPrec :: (Level p -> p) -> p
  localPrec :: (Level p -> Level p) -> p -> p
