{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Prec
( -- * Printing with precedence
  PrecPrinter(..)
, setPrec
, prec
, assoc
, nonAssoc
, leftAssoc
, rightAssoc
, infix_
  -- * Prec
, runPrec
, Prec(..)
) where

import Silkscreen

-- | Pretty-printing with parenthesis insertion resolving precedence.
--
-- Given:
--
-- @
-- data ArithLevel = Bottom | Add | Mult | Exp | Top
--   deriving (Eq, Ord)
--
-- (+.) :: (PrecPrinter p, Level p ~ ArithLevel) => p -> p -> p
-- (+.) = 'assoc' Add ('surround' ('pretty' " + "))
-- infixl 6 +.
--
-- (*.) :: (PrecPrinter p, Level p ~ ArithLevel) => p -> p -> p
-- (*.) = 'assoc' Mult ('surround' ('pretty' " * "))
-- infixl 7 *.
--
-- (^.) :: (PrecPrinter p, Level p ~ ArithLevel) => p -> p -> p
-- (^.) = 'rightAssoc' Exp Top ('surround' ('pretty' " ^ "))
-- infixr 8 ^.
-- @
--
-- >>> runPrec Bottom (('pretty' "a" +. 'pretty' "b") *. 'pretty' "c" ^. ('pretty' "d" *. 'pretty' "e"))
-- (a + b) * c ^ (d * e)
class Printer p => PrecPrinter p where
  -- | The type used to represent precedence levels. This is defined as an associated type so that consumers can use e.g. symbolic representations of their DSL’s precedence levels instead of e.g. unsemantic 'Int's.
  --
  -- This type will usually be 'Ord'ered, but this isn’t strictly required so that other means of determining precedence can be provided.
  type Level p

  -- | Print informed by the current 'Level'.
  askingPrec :: (Level p -> p) -> p

  -- | Locally change the 'Level' in a printer.
  localPrec :: (Level p -> Level p) -> p -> p

-- | Set a constant precedence.
--
-- This function does not insert parentheses, and thus should be used when inserting parentheses or otherwise resetting the precedence level.
setPrec :: PrecPrinter p => Level p -> p -> p
setPrec = localPrec . const

-- | Set a constant precedence, parenthesizing in higher-precedence contexts.
prec :: (PrecPrinter p, Ord (Level p)) => Level p -> p -> p
prec l d = askingPrec $ \ l' -> setPrec l (parensIf (l' > l) d)


-- | Make an associative infix combinator at the given level.
assoc :: (PrecPrinter p, Ord (Level p)) => Level p -> (p -> p -> p) -> (p -> p -> p)
assoc pout = infix_ pout id id

-- | Make a non-associative infix combinator at the given levels for the operator itself and its operands.
nonAssoc :: (PrecPrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
nonAssoc pout pin = infix_ pout (prec pin) (prec pin)

-- | Make a left-associative infix combinator at the given levels for the operator itself and its right operand.
leftAssoc :: (PrecPrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
leftAssoc pl pr = infix_ pl id (prec pr)

-- | Make a right-associative infix combinator at the given levels for the operator itself and its left operand.
rightAssoc :: (PrecPrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
rightAssoc pl pr = infix_ pr (prec pl) id

-- | Make an infix combinator at the given level for the operator itself, applying functions to either operand.
infix_ :: (PrecPrinter p, Ord (Level p)) => Level p -> (p -> p) -> (p -> p) -> (p -> p -> p) -> (p -> p -> p)
infix_ p fl fr op l r = prec p $ fl l `op` fr r


instance PrecPrinter b => PrecPrinter (a -> b) where
  type Level (a -> b) = Level b

  askingPrec f = askingPrec . flip f
  localPrec f p = localPrec f . p


-- Prec

runPrec :: level -> Prec level a -> a
runPrec level (Prec run) = run level

newtype Prec level a = Prec (level -> a)
  deriving (Applicative, Functor, Monad, Monoid, Semigroup)
