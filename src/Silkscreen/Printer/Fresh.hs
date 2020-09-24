module Silkscreen.Printer.Fresh
( -- * Printer binding fresh variables
  runFresh
, Fresh(..)
  -- * Re-exports
, module Silkscreen.Fresh
) where

import Silkscreen.Fresh

runFresh :: Int -> Fresh p -> p
runFresh v (Fresh run) = run v

newtype Fresh p = Fresh (Int -> p)
