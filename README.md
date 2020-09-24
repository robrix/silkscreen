# silkscreen: prettyprinting transformers

🎩 [@3_lemma](https://twitter.com/3_lemma/status/1304570835189915648) for the name!

Silkscreen abstracts the pretty-printing interface of [`prettyprinter`][] with a `Printer` typeclass, allowing the definition of pretty-printer _transformers_, layering new behaviours onto the existing primitives. For example, `Silkscreen.Rainbow` implements rainbow-parentheses, `Silkscreen.Prec` implements support for precedence à la `Show`, and `Silkscreen.Fresh` implements fresh variable generation suitable for pretty-printing ASTs with generated names.

[`prettyprinter`]: https://hackage.haskell.org/package/prettyprinter
