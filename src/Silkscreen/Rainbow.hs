{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Rainbow
( -- * Printing with nesting levels
  NestingPrinter(..)
, incrNesting
, encloseNesting
  -- * Rainbow parentheses
, runRainbow
, Rainbow(..)
) where

import Control.Applicative (liftA2)
import Control.Monad (liftM, ap)
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


runRainbow :: Handler -> Int -> Rainbow ann a -> a
runRainbow as l (Rainbow run) = run as l

type Handler = forall p . Printer p => Int -> p -> p

newtype Rainbow ann a = Rainbow (Handler -> Int -> a)
  deriving (Monoid, Semigroup)

applyNesting :: Printer a => Rainbow ann a -> Rainbow ann a
applyNesting a = Rainbow $ \ h l -> h l (runRainbow h l a)

instance Functor (Rainbow ann) where
  fmap = liftM

instance Applicative (Rainbow ann) where
  pure a = Rainbow $ \ _ _ -> a
  (<*>) = ap

instance Monad (Rainbow ann) where
  m >>= f = Rainbow $ \ as l -> runRainbow as l (f (runRainbow as l m))

instance Show a => Show (Rainbow ann a) where
  showsPrec p = showsPrec p . runRainbow (flip const) 0

instance (Printer a, Ann a ~ ann) => Printer (Rainbow ann a) where
  type Ann (Rainbow ann a) = ann

  fromDoc = pure . fromDoc
  annotate = fmap . annotate

  group = fmap group
  flatAlt = liftA2 flatAlt

  align = fmap align
  nest i = fmap (nest i)

  parens   = encloseNesting (applyNesting lparen)   (applyNesting rparen)
  brackets = encloseNesting (applyNesting lbracket) (applyNesting rbracket)
  braces   = encloseNesting (applyNesting lbrace)   (applyNesting rbrace)

instance (Printer a, Ann a ~ ann) => NestingPrinter (Rainbow ann a) where
  askingNesting f = Rainbow (\ as -> runRainbow as <*> f)
  localNesting f (Rainbow p) = Rainbow (\ as -> p as . f)
