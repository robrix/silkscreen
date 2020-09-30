{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Printer.Rainbow
( -- * Rainbow parentheses
  runRainbow
, Rainbow(..)
  -- * Re-exports
, module Silkscreen.Nesting
) where

import Data.Functor.Classes
import Silkscreen.Fresh
import Silkscreen.Nesting
import Silkscreen.Precedence

runRainbow :: (Int -> p a -> p a) -> Int -> Rainbow p a -> p a
runRainbow h l (Rainbow run) = run h l

newtype Rainbow p a = Rainbow ((Int -> p a -> p a) -> Int -> p a)
  deriving (Monoid, Semigroup)

instance (Show1 p, Show a) => Show (Rainbow p a) where
  showsPrec = showsPrec1

instance Show1 p => Show1 (Rainbow p) where
  liftShowsPrec sp sl p = liftShowsPrec sp sl p . runRainbow (flip const) 0

instance Printer p => Printer (Rainbow p) where
  liftDoc0 d = Rainbow $ \ _ _ -> liftDoc0 d
  liftDoc1 f p = Rainbow $ \ h l -> liftDoc1 f (runRainbow h l p)
  liftDoc2 f p1 p2 = Rainbow $ \ h l -> liftDoc2 f (runRainbow h l p1) (runRainbow h l p2)

  parens   = encloseNesting lparen   rparen
  brackets = encloseNesting lbracket rbracket
  braces   = encloseNesting lbrace   rbrace

instance Printer p => NestingPrinter (Rainbow p) where
  askingNesting f = Rainbow (\ as -> runRainbow as <*> f)
  localNesting f (Rainbow p) = Rainbow (\ as -> p as . f)
  applyNesting a = Rainbow $ \ h l -> h l (runRainbow h l a)

instance PrecedencePrinter p => PrecedencePrinter (Rainbow p) where
  type Level (Rainbow p) = Level p

  askingPrec f = Rainbow $ \ h l -> askingPrec (runRainbow h l . f)

  localPrec f (Rainbow p) = Rainbow $ \ h -> localPrec f . p h

instance FreshPrinter p => FreshPrinter (Rainbow p) where
  bind f = Rainbow $ \ h l -> bind (runRainbow h l . f)
