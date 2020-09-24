{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Rainbow
( -- * Printing with nesting levels
  RainbowPrinter(..)
  -- * Rainbow parentheses
, rainbow
, Rainbow(..)
) where

import Control.Applicative (liftA2)
import Silkscreen

class Printer p => RainbowPrinter p where
  -- | Increment the nesting level of a printer.
  --
  -- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
  incrNesting :: p -> p


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

  parens   = fmap parens   . incrNesting
  brackets = fmap brackets . incrNesting
  braces   = fmap braces   . incrNesting

instance Printer a => RainbowPrinter (Rainbow a) where
  incrNesting p = Rainbow (runRainbow p . succ)
