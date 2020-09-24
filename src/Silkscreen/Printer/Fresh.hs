{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Printer.Fresh
( -- * Printer binding fresh variables
  runFresh
, Fresh(..)
  -- * Re-exports
, module Silkscreen.Fresh
) where

import Control.Applicative (liftA2)
import Silkscreen.Fresh

runFresh :: Int -> Fresh p -> p
runFresh v (Fresh run) = run v

newtype Fresh p = Fresh (Int -> p)
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Show p => Show (Fresh p) where
  showsPrec p = showsPrec p . runFresh 0

instance Printer p => Printer (Fresh p) where
  type Ann (Fresh p) = Ann p

  liftDoc0 = pure . liftDoc0
  liftDoc1 = fmap . liftDoc1
  liftDoc2 = liftA2 . liftDoc2

  parens = fmap parens
  brackets = fmap brackets
  braces = fmap braces

instance Printer p => FreshPrinter (Fresh p) where
  bind f = Fresh $ \ v -> runFresh (succ v) (f v)
