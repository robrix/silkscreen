{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Rainbow
( -- * Printing with nesting levels
  RainbowPrinter(..)
  -- * Rainbow parentheses
, runRainbow
, Rainbow(..)
) where

import Control.Applicative (liftA2)
import Silkscreen

class Printer p => RainbowPrinter p where
  -- | Increment the nesting level of a printer.
  --
  -- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
  incrNesting :: p -> p


runRainbow :: Int -> Rainbow a -> a
runRainbow n (Rainbow run) = run n

newtype Rainbow a = Rainbow (Int -> a)
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Printer a => Printer (Rainbow a) where
  type Ann (Rainbow a) = Ann a

  fromDoc = pure . fromDoc
  annotate = fmap . annotate

  group = fmap group
  flatAlt = liftA2 flatAlt

  align = fmap align
  nest i = fmap (nest i)

  parens   = fmap parens
  brackets = fmap brackets
  braces   = fmap braces
