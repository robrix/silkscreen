{-# LANGUAGE TypeFamilies #-}
module Silkscreen
( Printer(..)
) where

class Printer p where
  type Ann p
