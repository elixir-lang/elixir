# Unicode Security

(See [Unicode Syntax](unicode-security.html) for more information on Unicode usage in Elixir).

Elixir will warn on confusing or suspicious uses of Unicode in identifiers since Elixir v1.15, as defined in the [Unicode Technical Standard #39](https://unicode.org/reports/tr39/) on Security.

The focus of this document is to describe how Elixir implements the conformance clauses from that standard, referred to as C1, C2, and so on. All quotes are from the spec unless otherwise noted.

## C1. General Security Profile for Identifiers

Elixir will issue 'uncommon codepoint' warnings on identifiers with codepoints in `\p{Identifier_Status=Restricted}`

> An implementation following the General Security Profile does not permit any characters in \p{Identifier_Status=Restricted}, ...

For instance, the 'HANGUL FILLER' (`ㅤ`) character, which is often invisible, is an uncommon codepoint and will trigger this warning.

Elixir also allows some Restricted codepoints by explicit lists.

> ... unless it documents the additional characters that it does allow. Such documentation can specify characters via properties, such as \p{Identifier_Status=Technical}, or by explicit lists, or by combinations of these.

Currently there is a whitelist of mathematical characters, which may be expanded over time, which can be used in identifier names:

    λ∆Ø∂δω∇Φϕσμπκxαθ

The C1 check is implemented via the `String.Unicode.Security.UncommonCodepoints` module. The name 'uncommon codepoints' reflects the reason for this check given in the spec, which is more meaninful than 'general profile':

> The Restricted characters are characters not in common use


## C2. Confusable detection

Elixir will warn on identifiers that look the same, but aren't. As an example, in `а = a = 1`, the two 'a' characters are Cyrillic and Latin, and could be confused for each other. Confusable identifiers can lead to hard-to-catch bugs (say, in copy-pasted code) and can represent an attack vector, so we will warn about identifiers within a single file that could be confused with each other.

The means used for confusable detection are those of Section 4, Confusable Detection, with one noted modification

> Alternatively, it shall declare that it uses a modification, and provide a precise list of character mappings that are added to or removed from the provided ones.

Elixir will not warn on confusability for identifiers made up exclusively of characters in a-z, A-Z, 0-9, and _. This is because ASCII identifiers have existed for so long that the programming community has had their own means of dealing with confusability between identifiers like `l,1` or `O,0`.

## C3. Mixed script detection

Elixir will warn on identifiers that contain a mix of characters from different scripts, but only when those scripts are not normally used together in a writing system.

* Some languages naturally use multiple scripts. For instance, the Japanese writing system may use multiple scripts, like Hiragana, Katakana, and Han -- so an identifier in Elixir could be comprised of characters from all of those scripts (as well as Common characters, like _ and 0-9; see below).

* Some letters may be used in multiple writing systems; for instance, a codepoint could appear in scripts used in the Japanese, Korean, and Chinese writing systems.

* However, there is no writing system that mixes Cyrillic and Latin characters, or so if that occurs Elixir will emit a warning on that identifier.

* Some characters are in use in so many writing systems that they have been classified by Unicode as 'Common' or 'Inherited'; these include things like numbers, underscores, etc; Elixir will not warn about mixing of ALL-script characters, like `幻ㄒㄧㄤ1 = :foo; 幻ㄒㄧㄤ2 = :bar`.



## C4, C5 (inapplicable)

'C4 - Restriction Level detection' conformance is inapplicable to identifiers and is NOT claimed; it applies to classifying the level of safety of a given arbitrary string into one of 5 restriction levels.

'C5 - Mixed number detection' conformance is not claimed. However, Mixed Script and Confusable detections provide a level of safety regarding most confusing or other uses of mixed-script numbers. For instance, the example in section 5.3 of BENGALI DIGIT FOUR (৪) with DIGIT EIGHT (8); this example: `utf৪ = true` would produce a mixed-script warning.
