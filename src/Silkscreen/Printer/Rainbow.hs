{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Printer.Rainbow
( -- * Rainbow parentheses
  runRainbow
, Rainbow(..)
  -- * Re-exports
, module Silkscreen.Nesting
) where

import Silkscreen.Nesting
import Silkscreen.Precedence

runRainbow :: (Int -> a -> a) -> Int -> Rainbow a -> a
runRainbow h l (Rainbow run) = run h l

newtype Rainbow a = Rainbow ((Int -> a -> a) -> Int -> a)
  deriving (Monoid, Semigroup)

instance Show a => Show (Rainbow a) where
  showsPrec p = showsPrec p . runRainbow (flip const) 0

instance Printer a => Printer (Rainbow a) where
  type Ann (Rainbow a) = Ann a

  liftDoc0 d = Rainbow $ \ _ _ -> liftDoc0 d
  liftDoc1 f p = Rainbow $ \ h l -> liftDoc1 f (runRainbow h l p)
  liftDoc2 f p1 p2 = Rainbow $ \ h l -> liftDoc2 f (runRainbow h l p1) (runRainbow h l p2)

  parens   = encloseNesting lparen   rparen
  brackets = encloseNesting lbracket rbracket
  braces   = encloseNesting lbrace   rbrace
  angles   = encloseNesting langle   rangle

  column    f = Rainbow $ \ h l -> column    (runRainbow h l . f)
  nesting   f = Rainbow $ \ h l -> nesting   (runRainbow h l . f)
  pageWidth f = Rainbow $ \ h l -> pageWidth (runRainbow h l . f)

instance Printer a => NestingPrinter (Rainbow a) where
  askingNesting f = Rainbow (\ as -> runRainbow as <*> f)
  localNesting f (Rainbow p) = Rainbow (\ as -> p as . f)
  applyNesting a = Rainbow $ \ h l -> h l (runRainbow h l a)

instance PrecedencePrinter p => PrecedencePrinter (Rainbow p) where
  type Level (Rainbow p) = Level p

  askingPrec f = Rainbow $ \ h l -> askingPrec (runRainbow h l . f)

  localPrec f (Rainbow p) = Rainbow $ \ h -> localPrec f . p h
