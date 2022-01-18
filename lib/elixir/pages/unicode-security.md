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

## C3. Mixed script detection

Elixir will warn on identifiers that contain a mix of characters from different scripts, but only if all of the following conditions are met:

1. when those scripts are not normally used together in a writing system, as defined by Unicode's definition of 'augmented script set' and 'resolved script set'
2. when usage of that script is comprised solely of characters that are 'mixed script confusable', per Unicode's definition of that term (confusable with characters in other scripts)

The following will NOT trigger mixed-script confusable warnings:

* Some languages naturally use multiple scripts. For instance, the Japanese writing system may use multiple scripts, like Hiragana, Katakana, and Han -- so an identifier in Elixir could be comprised of characters from all of those scripts (as well as Common characters, like _ and 0-9; see below).

* Some letters may be used in multiple writing systems; for instance, a codepoint could appear in scripts used in the Japanese, Korean, and Chinese writing systems.

* Some characters are in use in so many writing systems that they have been classified by Unicode as 'Common' or 'Inherited'; these include things like numbers, underscores, etc; Elixir will not warn about mixing of ALL-script characters, like `幻ㄒㄧㄤ1 = :foo; 幻ㄒㄧㄤ2 = :bar`.

However, there are some script combinations with no overlap in characters, like {Cyrillic} and {Latin} -- in Unicode terms, the 'resolved script set' would be empty. So if that kind of script mixing occurs in an identifier, and the only Cyrillic characters in the file are those confusable with characters in other languages, it will emit a warning to that effect. (If, however, the file also contains non-confusable Cyrillic characters in source code, then the programmer can visually detect that another script is being used, and no warning is issued).

## C4, C5 (inapplicable)

'C4 - Restriction Level detection' conformance is not claimed and does not apply to identifiers in code; rather, it applies to classifying the level of safety of a given arbitrary string into one of 5 restriction levels.

'C5 - Mixed number detection' conformance is inapplicable as Elixir does not support Unicode numbers.
