{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Printer.Prec
( -- * Precedence printer
  runPrec
, Prec(..)
  -- * Re-exports
, module Silkscreen.Precedence
) where

import Control.Applicative (liftA2)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor.Classes
import Silkscreen.Fresh
import Silkscreen.Nesting
import Silkscreen.Precedence

-- Prec

runPrec :: level -> Prec level p a -> p a
runPrec level (Prec run) = run level

newtype Prec level p a = Prec (level -> p a)
  deriving (Monoid, Semigroup)
  deriving (Applicative, Functor, Monad) via ReaderT level p
  deriving (MonadTrans) via ReaderT level

instance (Bounded level, Show1 p, Show a) => Show (Prec level p a) where
  showsPrec = showsPrec1

instance (Bounded level, Show1 p) => Show1 (Prec level p) where
  liftShowsPrec sp sl p = liftShowsPrec sp sl p . runPrec minBound

instance (Bounded level, Printer p) => Printer (Prec level p) where
  liftDoc0 = Prec . const . liftDoc0
  liftDoc1 f (Prec r) = Prec $ liftDoc1 f . r
  liftDoc2 f (Prec r1) (Prec r2) = Prec $ liftA2 (liftDoc2 f) r1 r2

  parens   = mapPrec parens   . setPrec minBound
  brackets = mapPrec brackets . setPrec minBound
  braces   = mapPrec braces   . setPrec minBound

instance (Bounded level, Printer a) => PrecedencePrinter (Prec level a) where
  type Level (Prec level a) = level

  askingPrec f = Prec (runPrec <*> f)
  localPrec f (Prec p) = Prec (p . f)

instance (Bounded level, NestingPrinter p) => NestingPrinter (Prec level p) where
  askingNesting f = Prec $ \ level -> askingNesting (runPrec level . f)

  localNesting f (Prec p) = Prec $ localNesting f . p

  applyNesting (Prec p) = Prec $ applyNesting . p

instance (Bounded level, FreshPrinter p) => FreshPrinter (Prec level p) where
  bind f = Prec $ \ l -> bind (runPrec l . f)


mapPrec :: (p a -> p' a') -> Prec level p a -> Prec level p' a'
mapPrec f (Prec r) = Prec $ f . r
