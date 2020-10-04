# silkscreen: prettyprinting transformers

[![hackage](https://img.shields.io/hackage/v/silkscreen.svg?color=blue&style=popout)](http://hackage.haskell.org/package/silkscreen)

🎩 [@3_lemma](https://twitter.com/3_lemma/status/1304570835189915648) for the name!

Silkscreen abstracts the pretty-printing interface of [`prettyprinter`][] with a `Printer` typeclass, allowing the definition of composable pretty-printer _transformers_, layering new behaviours onto the existing primitives. For example, `Silkscreen.Prec` implements support for precedence à la `showsPrec`, but using symbolic precedence levels, and `Silkscreen.Rainbow` implements rainbow-parentheses.

[`prettyprinter`]: https://hackage.haskell.org/package/prettyprinter
