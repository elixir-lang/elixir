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

## C3. Mixed Script Detection

Elixir will not allow tokenization of mixed-script identifiers unless the mixings is one of the exceptions defined in UTS 39 5.2, 'Highly Restrictive'. We use the means described in Section 5.1, Mixed-Script Detection, to determine if script mixing is occurring.

Examples: Elixir allows an identifiers like `幻ㄒㄧㄤ`, even though it includes characters from multiple 'scripts', because those scripts all 'resolve' to Japanese when applying the resolution rules from UTS 39 5.1. It also allows an atom like `:Tシャツ`, the Japanese word for 't-shirt', which incorporates a Latin capital T, because {Latn, Jpan} is one of the allowed script mixings in the definition of 'Highly Restrictive' in UTS 39 5.2, and it 'covers' the string.

However, Elixir would prevent tokenization in code like `if аdmin, do: :ok, else: :err`, where the scriptset for the 'a' character is {Cyrillic} but all other characters have scriptsets of {Latin}. The scriptsets fail to resolve, and the scriptsets from the definition of 'Highly Restrictive' in UTS 39 5.2 do not cover the string either, so a descriptive error is shown.

## C4, C5 (inapplicable)

'C4 - Restriction Level detection' conformance is not claimed and does not apply to identifiers in code; rather, it applies to classifying the level of safety of a given arbitrary string into one of 5 restriction levels.

'C5 - Mixed number detection' conformance is inapplicable as Elixir does not support Unicode numbers.
