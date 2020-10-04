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
import Silkscreen.Nesting
import Silkscreen.Precedence

-- Prec

runPrec :: level -> Prec level a -> a
runPrec level (Prec run) = run level

newtype Prec level a = Prec (level -> a)
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance (Bounded level, Show a) => Show (Prec level a) where
  showsPrec p = showsPrec p . runPrec minBound

instance (Bounded level, Printer a) => Printer (Prec level a) where
  type Ann (Prec level a) = Ann a

  liftDoc0 = pure . liftDoc0
  liftDoc1 = fmap . liftDoc1
  liftDoc2 = liftA2 . liftDoc2

  enclosing l r x = enclose <$> l <*> r <*> setPrec minBound x

  column    f = Prec $ \ l -> column    (runPrec l . f)
  nesting   f = Prec $ \ l -> nesting   (runPrec l . f)
  pageWidth f = Prec $ \ l -> pageWidth (runPrec l . f)

instance (Bounded level, Printer a) => PrecedencePrinter (Prec level a) where
  type Level (Prec level a) = level

  askingPrec f = Prec (runPrec <*> f)
  localPrec f (Prec p) = Prec (p . f)

instance (Bounded level, NestingPrinter p) => NestingPrinter (Prec level p) where
  askingNesting f = Prec $ \ level -> askingNesting (runPrec level . f)

  localNesting f (Prec p) = Prec $ localNesting f . p

  applyNesting (Prec p) = Prec $ applyNesting . p
