{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Prec
( -- * Printing with precedence
  PrecedencePrinter(..)
, setPrec
, prec
, assoc
, nonAssoc
, leftAssoc
, rightAssoc
, infix_
  -- * Re-exports
, module Silkscreen
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
-- (+.) :: (PrecedencePrinter p, Level p ~ ArithLevel) => p -> p -> p
-- (+.) = 'assoc' Add ('surround' ('pretty' " + "))
-- infixl 6 +.
--
-- (*.) :: (PrecedencePrinter p, Level p ~ ArithLevel) => p -> p -> p
-- (*.) = 'assoc' Mult ('surround' ('pretty' " * "))
-- infixl 7 *.
--
-- (^.) :: (PrecedencePrinter p, Level p ~ ArithLevel) => p -> p -> p
-- (^.) = 'rightAssoc' Exp Top ('surround' ('pretty' " ^ "))
-- infixr 8 ^.
-- @
--
-- >>> putDoc . runPrec Bottom $ ('pretty' "a" +. 'pretty' "b") *. 'pretty' "c" ^. ('pretty' "d" *. 'pretty' "e")
-- (a + b) * c ^ (d * e)
class Printer p => PrecedencePrinter p where
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
setPrec :: PrecedencePrinter p => Level p -> p -> p
setPrec = localPrec . const

-- | Set a constant precedence, parenthesizing in higher-precedence contexts.
prec :: (PrecedencePrinter p, Ord (Level p)) => Level p -> p -> p
prec l d = askingPrec $ \ l' -> setPrec l (parensIf (l' > l) d)


-- | Make an associative infix combinator at the given level.
assoc :: (PrecedencePrinter p, Ord (Level p)) => Level p -> (p -> p -> p) -> (p -> p -> p)
assoc pout = infix_ pout id id

-- | Make a non-associative infix combinator at the given levels for the operator itself and its operands.
nonAssoc :: (PrecedencePrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
nonAssoc pout pin = infix_ pout (prec pin) (prec pin)

-- | Make a left-associative infix combinator at the given levels for the operator itself and its right operand.
leftAssoc :: (PrecedencePrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
leftAssoc pl pr = infix_ pl id (prec pr)

-- | Make a right-associative infix combinator at the given levels for the operator itself and its left operand.
rightAssoc :: (PrecedencePrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
rightAssoc pr pl = infix_ pr (prec pl) id

-- | Make an infix combinator at the given level for the operator itself, applying functions to either operand.
infix_ :: (PrecedencePrinter p, Ord (Level p)) => Level p -> (p -> p) -> (p -> p) -> (p -> p -> p) -> (p -> p -> p)
infix_ p fl fr op l r = prec p $ fl l `op` fr r


instance PrecedencePrinter b => PrecedencePrinter (a -> b) where
  type Level (a -> b) = Level b

  askingPrec f = askingPrec . flip f
  localPrec f p = localPrec f . p
