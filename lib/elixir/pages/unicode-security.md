# Unicode Security

(See [Unicode Syntax](unicode-syntax.html) for information on Unicode usage in Elixir).

Elixir will prevent, or warn on, confusing or suspicious uses of Unicode in identifiers since Elixir v1.15, as defined in the [Unicode Technical Standard #39](https://unicode.org/reports/tr39/) on Security.

The focus of this document is to describe how Elixir implements the conformance clauses from that standard, referred to as C1, C2, and so on. All quotes are from the spec unless otherwise noted.

## C1. General Security Profile for Identifiers

Elixir will not allow tokenization of identifiers with codepoints in `\p{Identifier_Status=Restricted}`.

> An implementation following the General Security Profile does not permit any characters in \p{Identifier_Status=Restricted}, ...

For instance, the 'HANGUL FILLER' (`ㅤ`) character, which is often invisible, is an uncommon codepoint and will trigger this warning.

## C2, C3 (planned)

Elixir may implement Confusable Detection, and Mixed-Script Confusable detection, in the future, and will likely emit warnings in those cases; there is a reference implementation.

## C4, C5 (inapplicable)

'C4 - Restriction Level detection' conformance is not claimed and is inapplicable. (It applies to classifying the level of safety of a given arbitrary string into one of 5 restriction levels).

'C5 - Mixed number detection' conformance is inapplicable as Elixir does not support Unicode numbers.
