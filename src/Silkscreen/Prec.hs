{-# LANGUAGE TypeFamilies #-}
module Silkscreen.Prec
( PrecPrinter(..)
, setPrec
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

setPrec :: PrecPrinter p => Level p -> p -> p
setPrec = localPrec . const
