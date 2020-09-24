module Silkscreen.Printer.Fresh
( -- * Printer binding fresh variables
  runFresh
, Fresh(..)
) where

runFresh :: Int -> Fresh p -> p
runFresh v (Fresh run) = run v

newtype Fresh p = Fresh (Int -> p)
