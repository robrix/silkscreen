module Silkscreen.Printer.Fresh
( -- * Printer binding fresh variables
  Fresh(..)
) where

newtype Fresh p = Fresh (Int -> p)
