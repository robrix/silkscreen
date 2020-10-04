{-# LANGUAGE TypeFamilies #-}
-- | Silkscreen is a library of pretty-printing transformers built around the @prettyprinter@ package. This module defines the core 'Printer' abstraction and a few instances.
--
-- More documentation can be found in "Prettyprinter".
module Silkscreen
( -- * Printing
  Printer(..)
  -- * Combinators
, pretty
, annotate
, group
, flatAlt
, align
, hang
, indent
, nest
, concatWith
, hsep
, vsep
, fillSep
, sep
, hcat
, vcat
, fillCat
, cat
, punctuate
, width
, fill
, fillBreak
, plural
, enclose
, encloseSep
, list
, tupled
, surround
, (<+>)
, (</>)
  -- ** Conditional combinators
, parensIf
  -- * Symbols
, space
, line
, line'
, softline
, softline'
, hardline
, lparen
, rparen
, lbracket
, rbracket
, lbrace
, rbrace
, langle
, rangle
, comma
, colon
  -- * Re-exports
, P.Pretty
, P.PageWidth(..)
) where

import           Control.Applicative (liftA2)
import           Data.Semigroup (stimes)
import qualified Prettyprinter as P

-- | A 'Printer' abstracts pretty-printing to allow the composition of behaviours such as e.g. rainbow parentheses, precedence handling, and so forth.
class Monoid p => Printer p where
  -- | The type of annotations supported by the printer.
  --
  -- We provide this as a type family instead of defining 'Printer' over kind @Type -> Type@ in order to allow instances to constrain annotations.
  type Ann p

  -- | Lift a 'P.Doc' to a 'Printer'.
  liftDoc0 :: P.Doc (Ann p) -> p

  -- | Lift a unary function on 'P.Doc' to a 'Printer'.
  liftDoc1 :: (P.Doc (Ann p) -> P.Doc (Ann p)) -> (p -> p)

  -- | Lift a binary function on 'P.Doc' to a 'Printer'.
  liftDoc2 :: (P.Doc (Ann p) -> P.Doc (Ann p) -> P.Doc (Ann p)) -> (p -> p -> p)


  -- | Parenthesize the argument.
  --
  -- Overloadable to support e.g. rainbow parentheses.
  parens :: p -> p
  parens = enclose lparen rparen

  -- | Wrap the argument in brackets.
  --
  -- Overloadable to support e.g. rainbow brackets.
  brackets :: p -> p
  brackets = enclose lbracket rbracket

  -- | Wrap the argument in braces.
  --
  -- Overloadable to support e.g. rainbow braces.
  braces :: p -> p
  braces = enclose lbrace rbrace

  -- | Wrap the argument in braces.
  --
  -- Overloadable to support e.g. rainbow angle brackets.
  angles :: p -> p
  angles = enclose langle rangle


  column :: (Int -> p) -> p

  nesting :: (Int -> p) -> p

  pageWidth :: (P.PageWidth -> p) -> p


-- Non-primitive combinators

-- | Pretty-print a value using the 'P.Pretty' instance for its type.
pretty :: (Printer p, P.Pretty t) => t -> p
pretty = liftDoc0 . P.pretty


-- | Annotate a 'Printer' with an @'Ann' p@.
annotate :: Printer p => Ann p -> p -> p
annotate = liftDoc1 . P.annotate


-- | Try to unwrap the argument, if it will fit.
group :: Printer p => p -> p
group = liftDoc1 P.group

-- | Print the first argument by default, or the second when an enclosing 'group' flattens it.
flatAlt :: Printer p => p -> p -> p
flatAlt = liftDoc2 P.flatAlt


-- | Indent lines in the argument to the current column.
align :: Printer p => p -> p
align = liftDoc1 P.align

-- | Indent following lines in the argument to the current column + some delta.
hang :: Printer p => Int -> p -> p
hang = liftDoc1 . P.hang

-- | Indent lines in the argument to the current column + some delta.
indent :: Printer p => Int -> p -> p
indent = liftDoc1 . P.indent


-- | @'nest' i p@ changes the indentation level for new lines in @p@ by @i@.
nest :: Printer p => Int -> p -> p
nest = liftDoc1 . P.nest


concatWith :: (Monoid p, Foldable t) => (p -> p -> p) -> t p -> p
concatWith (<>) ds
  | null ds   = mempty
  | otherwise = foldr1 (<>) ds


hsep :: Printer p => [p] -> p
hsep = concatWith (<+>)

vsep :: Printer p => [p] -> p
vsep = concatWith (</>)

fillSep :: Printer p => [p] -> p
fillSep = concatWith (surround softline)

sep :: Printer p => [p] -> p
sep = group . vsep


hcat :: Printer p => [p] -> p
hcat = mconcat

vcat :: Printer p => [p] -> p
vcat = concatWith (surround line')

fillCat :: Printer p => [p] -> p
fillCat = concatWith (surround softline')

cat :: Printer p => [p] -> p
cat = group . vcat


punctuate :: Printer p => p -> [p] -> [p]
punctuate s = go
  where
  go []     = []
  go [x]    = [x]
  go (x:xs) = x <> s : go xs


width :: Printer p => p -> (Int -> p) -> p
width p f = column $ \ start -> p <> column (\ end -> f (end - start))


fill :: Printer p => Int -> p -> p
fill n p = width p $ \ w -> stimes (n - w) space

fillBreak :: Printer p => Int -> p -> p
fillBreak f x = width x go
  where
  go w
    | w > f = nest f line'
    | otherwise = stimes (f - w) space

plural :: (Num n, Eq n) => p -> p -> n -> p
plural _1 _n n
  | n == 1    = _1
  | otherwise = _n


-- | @'enclose' l r x@ wraps @x@ in @l@ and @r@.
enclose :: Printer p => p -> p -> p -> p
enclose l r x = l <> x <> r

encloseSep :: Printer p => p -> p -> p -> [p] -> p
encloseSep l r s ps = enclose l r (group (concatWith (surround (line' <> s)) ps))

list :: Printer p => [p] -> p
list
  = group
  . brackets
  . encloseSep
    (flatAlt space mempty)
    (flatAlt space mempty)
    (comma <> space)

tupled :: Printer p => [p] -> p
tupled
  = group
  . parens
  . encloseSep
    (flatAlt space mempty)
    (flatAlt space mempty)
    (comma <> space)


-- | @'surround' x l r@ wraps @x@ in @l@ and @r@.
--
-- This is a reordering of 'enclose', but allows for convenient use in e.g. folds:
--
-- >>> 'foldr1' ('surround' ('pretty' ", ")) ['pretty' "apple", 'pretty' "banana"]
-- apple, banana
surround :: Printer p => p -> p -> p -> p
surround x l r = enclose l r x

-- | Separate the arguments with a space.
(<+>) :: Printer p => p -> p -> p
(<+>) = surround space

infixr 6 <+>

-- | Separate the arguments with a line.
(</>) :: Printer p => p -> p -> p
(</>) = surround line

infixr 6 </>


-- | Conditional parenthesization of a printer. Analogous to 'showParen', but for printers.
parensIf :: Printer p => Bool -> p -> p
parensIf True = parens
parensIf _    = id


-- Symbols

space :: Printer p => p
space = liftDoc0 P.space

line :: Printer p => p
line = liftDoc0 P.line

line' :: Printer p => p
line' = liftDoc0 P.line'

softline :: Printer p => p
softline = liftDoc0 P.softline

softline' :: Printer p => p
softline' = liftDoc0 P.softline'

hardline :: Printer p => p
hardline = liftDoc0 P.hardline

lparen, rparen :: Printer p => p
lparen = liftDoc0 P.lparen
rparen = liftDoc0 P.rparen

lbracket, rbracket :: Printer p => p
lbracket = liftDoc0 P.lbracket
rbracket = liftDoc0 P.rbracket

lbrace, rbrace :: Printer p => p
lbrace = liftDoc0 P.lbrace
rbrace = liftDoc0 P.rbrace

langle, rangle :: Printer p => p
langle = liftDoc0 P.langle
rangle = liftDoc0 P.rangle

comma :: Printer p => p
comma = liftDoc0 P.comma

colon :: Printer p => p
colon = liftDoc0 P.colon


instance Printer (P.Doc ann) where
  type Ann (P.Doc ann) = ann

  liftDoc0 = id
  liftDoc1 = id
  liftDoc2 = id

  parens = P.parens
  brackets = P.brackets
  braces = P.braces

  column    = P.column
  nesting   = P.nesting
  pageWidth = P.pageWidth


instance (Printer a, Printer b, Ann a ~ Ann b) => Printer (a, b) where
  type Ann (a, b) = Ann b

  liftDoc0 d = (liftDoc0 d, liftDoc0 d)
  liftDoc1 f (a, b) = (liftDoc1 f a, liftDoc1 f b)
  liftDoc2 f (a1, b1) (a2, b2) = (liftDoc2 f a1 a2, liftDoc2 f b1 b2)

  parens (a, b) = (parens a, parens b)
  brackets (a, b) = (brackets a, brackets b)
  braces (a, b) = (braces a, braces b)

  column    f = (column    (fst . f), column    (snd . f))
  nesting   f = (nesting   (fst . f), nesting   (snd . f))
  pageWidth f = (pageWidth (fst . f), pageWidth (snd . f))


instance Printer b => Printer (a -> b) where
  type Ann (a -> b) = Ann b

  liftDoc0 = pure . liftDoc0
  liftDoc1 = fmap . liftDoc1
  liftDoc2 = liftA2 . liftDoc2

  parens = fmap parens
  brackets = fmap brackets
  braces = fmap braces

  column    f = column    . flip f
  nesting   f = nesting   . flip f
  pageWidth f = pageWidth . flip f
