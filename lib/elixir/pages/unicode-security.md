# Unicode Security

(See [Unicode Syntax](unicode-syntax.html) for information on Unicode usage in Elixir).

Elixir will prevent, or warn on, confusing or suspicious uses of Unicode in identifiers since Elixir v1.15, as defined in the [Unicode Technical Standard #39](https://unicode.org/reports/tr39/) on Security.

The focus of this document is to describe how Elixir implements the conformance clauses from that standard, referred to as C1, C2, and so on. All quotes are from the spec unless otherwise noted.

## C1. General Security Profile for Identifiers

Elixir will not allow tokenization of identifiers with codepoints in `\p{Identifier_Status=Restricted}`.

> An implementation following the General Security Profile does not permit any characters in \p{Identifier_Status=Restricted}, ...

For instance, the 'HANGUL FILLER' (`ㅤ`) character, which is often invisible, is an uncommon codepoint and will trigger this warning.

## C2. Confusable detection

Elixir will warn on identifiers that look the same, but aren't. Examples: in `а = a = 1`, the two 'a' characters are Cyrillic and Latin, and could be confused for each other; in `力 = カ = 1`, both are Japanese, but different codepoints, in different scripts of that writing system. Confusable identifiers can lead to hard-to-catch bugs (say, due to copy-pasted code) and can be unsafe, so we will warn about identifiers within a single file that could be confused with each other.

We use the means described in Section 4, 'Confusable Detection', with one noted modification

> Alternatively, it shall declare that it uses a modification, and provide a precise list of character mappings that are added to or removed from the provided ones.

Elixir will not warn on confusability for identifiers made up exclusively of characters in a-z, A-Z, 0-9, and _. This is because ASCII identifiers have existed for so long that the programming community has had their own means of dealing with confusability between identifiers like `l,1` or `O,0` (for instance, fonts designed for programming usually make it easy to differentiate between those characters).

## C3. (not yet implemented)

C3 has to do with detecting mixed-script-confusable characters -- like, say, a file in which several Cyrillic 'a' characters are present in a file of mostly latin identifiers. Conformance with this clause is not yet claimed.

## C4, C5 (inapplicable)

'C4 - Restriction Level detection' conformance is not claimed and does not apply to identifiers in code; rather, it applies to classifying the level of safety of a given arbitrary string into one of 5 restriction levels.

'C5 - Mixed number detection' conformance is inapplicable as Elixir does not support Unicode numbers.
