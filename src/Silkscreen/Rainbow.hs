{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Rainbow
( -- * Printing with nesting levels
  NestingPrinter(..)
, incrNesting
, encloseNesting
  -- * Rainbow parentheses
, rainbow
, Rainbow(..)
) where

import Control.Applicative (liftA2)
import Silkscreen

class Printer p => NestingPrinter p where
  -- | Make a printer informed by the current nesting level.
  askingNesting :: (Int -> p) -> p

  -- | Locally change the nesting level for a printer.
  localNesting :: (Int -> Int) -> p -> p

-- | Increment the nesting level of a printer.
--
-- This should be used inside parentheses, brackets, braces, etc., and will inform the annotation of their delimiters.
incrNesting :: NestingPrinter p => p -> p
incrNesting = localNesting succ

encloseNesting :: NestingPrinter p => p -> p -> p -> p
encloseNesting l r = enclose l r . incrNesting


rainbow :: Rainbow ann a -> a
rainbow (Rainbow run) = run 0

newtype Rainbow ann a = Rainbow { runRainbow :: Int -> a }
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)

instance Show a => Show (Rainbow ann a) where
  showsPrec p = showsPrec p . rainbow

instance (Printer a, Ann a ~ ann) => Printer (Rainbow ann a) where
  type Ann (Rainbow ann a) = ann

  fromDoc = pure . fromDoc
  annotate = fmap . annotate

  group = fmap group
  flatAlt = liftA2 flatAlt

  align = fmap align
  nest i = fmap (nest i)

  parens   = encloseNesting lparen   rparen
  brackets = encloseNesting lbracket rbracket
  braces   = encloseNesting lbrace   rbrace

instance (Printer a, Ann a ~ ann) => NestingPrinter (Rainbow ann a) where
  askingNesting f = Rainbow (flip runRainbow <*> f)
  localNesting f p = Rainbow (runRainbow p . f)
