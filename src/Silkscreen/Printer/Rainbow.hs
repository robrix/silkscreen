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

runRainbow :: (Int -> a -> a) -> Int -> Rainbow a -> a
runRainbow h l (Rainbow run) = run h l

newtype Rainbow a = Rainbow ((Int -> a -> a) -> Int -> a)
  deriving (Monoid, Semigroup)

instance Show a => Show (Rainbow a) where
  showsPrec p = showsPrec p . runRainbow (flip const) 0

instance Printer a => Printer (Rainbow a) where
  type Ann (Rainbow a) = Ann a

  liftDoc0 = liftR0 . liftDoc0
  liftDoc1 = liftR1 . liftDoc1
  liftDoc2 = liftR2 . liftDoc2

  parens   = encloseNesting lparen   rparen
  brackets = encloseNesting lbracket rbracket
  braces   = encloseNesting lbrace   rbrace

instance Printer a => NestingPrinter (Rainbow a) where
  askingNesting f = Rainbow (\ as -> runRainbow as <*> f)
  localNesting f (Rainbow p) = Rainbow (\ as -> p as . f)
  applyNesting a = Rainbow $ \ h l -> h l (runRainbow h l a)


-- | Like 'pure', w/o requiring an 'Applicative' instance.
liftR0 :: a -> Rainbow a
liftR0 a = Rainbow $ \ _ _ -> a

-- | Like a type-restricted 'fmap', w/o requiring a 'Functor' instance.
liftR1 :: (a -> a) -> (Rainbow a -> Rainbow a)
liftR1 f r = Rainbow $ \ h l -> f (runRainbow h l r)

-- | Like a type-restricted 'Control.Applicative.liftA2', w/o requiring an 'Applicative' instance.
liftR2 :: (a -> a -> a) -> (Rainbow a -> Rainbow a -> Rainbow a)
liftR2 f r1 r2 = Rainbow $ \ h l -> f (runRainbow h l r1) (runRainbow h l r2)
