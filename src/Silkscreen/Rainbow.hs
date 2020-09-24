{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Rainbow
( -- * Printing with nesting levels
  RainbowPrinter(..)
, encloseNesting
  -- * Rainbow parentheses
, rainbow
, Rainbow(..)
) where

import Control.Applicative (liftA2)
import Silkscreen

class Printer p => RainbowPrinter p where
  askingNesting :: (Int -> p) -> p

  -- | Increment the nesting level of a printer.
  --
  -- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
  incrNesting :: p -> p

encloseNesting :: RainbowPrinter p => p -> p -> p -> p
encloseNesting l r = enclose l r . incrNesting


rainbow :: Rainbow a -> a
rainbow (Rainbow run) = run 0

newtype Rainbow a = Rainbow { runRainbow :: Int -> a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Printer a => Printer (Rainbow a) where
  type Ann (Rainbow a) = Ann a

  fromDoc = pure . fromDoc
  annotate = fmap . annotate

  group = fmap group
  flatAlt = liftA2 flatAlt

  align = fmap align
  nest i = fmap (nest i)

  parens   = encloseNesting lparen   rparen
  brackets = encloseNesting lbracket rbracket
  braces   = encloseNesting lbrace   rbrace

instance Printer a => RainbowPrinter (Rainbow a) where
  askingNesting f = Rainbow (flip runRainbow <*> f)
  incrNesting p = Rainbow (runRainbow p . succ)
