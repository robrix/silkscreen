{-# LANGUAGE DerivingVia #-}
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
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor.Classes
import Silkscreen.Fresh
import Silkscreen.Nesting
import Silkscreen.Precedence

runFresh :: Int -> Fresh p a -> p a
runFresh v (Fresh run) = run v

newtype Fresh p a = Fresh (Int -> p a)
  deriving (Monoid, Semigroup)
  deriving (Applicative, Functor, Monad) via ReaderT Int p
  deriving (MonadTrans) via ReaderT Int

instance (Show1 p, Show a) => Show (Fresh p a) where
  showsPrec = showsPrec1

instance Show1 p => Show1 (Fresh p) where
  liftShowsPrec sp sl p = liftShowsPrec sp sl p . runFresh 0

instance Printer p => Printer (Fresh p) where
  liftDoc0 = Fresh . const . liftDoc0
  liftDoc1 = mapFresh . liftDoc1
  liftDoc2 f (Fresh r1) (Fresh r2) = Fresh $ liftA2 (liftDoc2 f) r1 r2

  parens = mapFresh parens
  brackets = mapFresh brackets
  braces = mapFresh braces

instance Printer p => FreshPrinter (Fresh p) where
  bind f = Fresh $ \ v -> runFresh (succ v) (f v)

instance NestingPrinter p => NestingPrinter (Fresh p) where
  askingNesting f = Fresh $ \ v -> askingNesting (runFresh v . f)

  localNesting f (Fresh p) = Fresh $ localNesting f . p

  applyNesting (Fresh p) = Fresh $ applyNesting . p

instance PrecedencePrinter p => PrecedencePrinter (Fresh p) where
  type Level (Fresh p) = Level p

  askingPrec f = Fresh $ \ v -> askingPrec (runFresh v . f)

  localPrec f (Fresh p) = Fresh $ localPrec f . p


mapFresh :: (p a -> p' a') -> Fresh p a -> Fresh p' a'
mapFresh f (Fresh r) = Fresh $ f . r
