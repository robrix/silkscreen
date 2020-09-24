{-# LANGUAGE FlexibleContexts #-}
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
) where

import Silkscreen

-- | Pretty-printing with parenthesis insertion resolving precedence.
--
-- Given:
--
-- @
-- data ArithLevel = Add | Mult | Exp
--   deriving (Eq, Ord)
--
-- (+.) :: (PrecPrinter p, Level p ~ ArithLevel) => p -> p -> p
-- a +. b = 'prec' Add $ a '<+>' 'pretty' "+" '<+>' b
-- infixl 6 +.
--
-- (*.) :: (PrecPrinter p, Level p ~ ArithLevel) => p -> p -> p
-- a *. b = 'prec' Mult $ a '<+>' 'pretty' "*" '<+>' b
-- infixl 7 *.
--
-- (^.) :: (PrecPrinter p, Level p ~ ArithLevel) => p -> p -> p
-- a ^. b = 'prec' Exp $ a '<+>' 'pretty' "^" '<+>' b
-- infixl 8 ^.
-- @
--
-- >>> (pretty "a" +. pretty "b") *. pretty "c" ^. (pretty "d" *. pretty "e")
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


assoc :: (PrecPrinter p, Ord (Level p)) => Level p -> (p -> p -> p) -> (p -> p -> p)
assoc pout = infix_ pout id id

nonAssoc :: (PrecPrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
nonAssoc pout pin = infix_ pout (prec pin) (prec pin)

leftAssoc :: (PrecPrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
leftAssoc pl pr = infix_ pl id (prec pr)

rightAssoc :: (PrecPrinter p, Ord (Level p)) => Level p -> Level p -> (p -> p -> p) -> (p -> p -> p)
rightAssoc pl pr = infix_ pr (prec pl) id

infix_ :: (PrecPrinter p, Ord (Level p)) => Level p -> (p -> p) -> (p -> p) -> (p -> p -> p) -> (p -> p -> p)
infix_ p fl fr op l r = prec p $ fl l `op` fr r


instance PrecPrinter b => PrecPrinter (a -> b) where
  type Level (a -> b) = Level b

  askingPrec f = askingPrec . flip f
  localPrec f p = localPrec f . p
