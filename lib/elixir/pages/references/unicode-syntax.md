<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Unicode syntax

Elixir supports Unicode throughout the language. This document is a complete reference of how
Elixir supports Unicode in its syntax.

Strings (`"olá"`) and charlists (`'olá'`) support Unicode since Elixir v1.0. Strings are UTF-8 encoded. Charlists are lists of Unicode code points. In such cases, the contents are kept as written by developers, without any transformation.

Elixir also supports Unicode in variables, atoms, and calls since Elixir v1.5. The focus of this document is to provide a high-level introduction to how Elixir allows Unicode in its syntax. We also provide technical documentation describing how Elixir complies with the Unicode specification.

To check the Unicode version of your current Elixir installation, run `String.Unicode.version()`.

## Introduction

Elixir allows Unicode characters in its variables, atoms, and calls. However, the Unicode characters must still obey the rules of the language syntax. In particular, variables and calls cannot start with an uppercase letter. From now on, we will refer to those terms as identifiers.

The characters allowed in identifiers are the ones specified by Unicode. Generally speaking, it is restricted to characters typically used by the writing system of human languages still in activity. In particular, it excludes symbols such as emojis, alternate numeric representations, musical notes, and the like.

Elixir imposes many restrictions on identifiers for security purposes. For example, the word "josé" can be written in two ways in Unicode: as the combination of the characters `j o s é` and as a combination of the characters `j o s e ́ `, where the accent is its own character. The former is called NFC form and the latter is the NFD form. Elixir normalizes all characters to be the in the NFC form.

Elixir also disallows mixed-scripts which are not explicitly separated by `_`. For example, it is not possible to name a variable `аdmin`, where `а` is in Cyrillic and the remaining characters are in Latin. Doing so will raise the following error:

```text
** (SyntaxError) invalid mixed-script identifier found: аdmin

Mixed-script identifiers are not supported for security reasons. 'аdmin' is made of the following scripts:

  \u0430 а {Cyrillic}
  \u0064 d {Latin}
  \u006D m {Latin}
  \u0069 i {Latin}
  \u006E n {Latin}

Make sure all characters in the identifier resolve to a single script or a highly
restrictive script. See https://hexdocs.pm/elixir/unicode-syntax.html for more information.
```

Finally, Elixir will also warn on confusable identifiers in the same file. For example, Elixir will emit a warning if you use both variables `а` (Cyrillic) and `а` (Latin) in your code.

That's the overall introduction of how Unicode is used in Elixir identifiers. In a nutshell, its goal is to support different writing systems in use today while keeping the Elixir language itself clear and secure.

For the technical details, see the next sections that cover the technical Unicode requirements.

## Unicode Annex #31

Elixir implements the requirements outlined in the [Unicode Annex #31](https://unicode.org/reports/tr31/), version 15.0.

### R1. Default Identifiers

The general Elixir identifier rule is specified as:

    <Identifier> := <Start> <Continue>* <Ending>?

where `<Start>` uses the same categories as the spec but normalizes them to the NFC form (see R4):

> characters derived from the Unicode General Category of uppercase letters, lowercase letters, titlecase letters, modifier letters, other letters, letter numbers, plus `Other_ID_Start`, minus `Pattern_Syntax` and `Pattern_White_Space` code points
>
> In set notation: `[\p{L}\p{Nl}\p{Other_ID_Start}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]`.

and `<Continue>` uses the same categories as the spec but normalizes them to the NFC form (see R4):

> ID_Start characters, plus characters having the Unicode General Category of nonspacing marks, spacing combining marks, decimal number, connector punctuation, plus `Other_ID_Continue`, minus `Pattern_Syntax` and `Pattern_White_Space` code points.
>
> In set notation: `[\p{ID_Start}\p{Mn}\p{Mc}\p{Nd}\p{Pc}\p{Other_ID_Continue}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]`.

`<Ending>` is an addition specific to Elixir that includes only the code points `?` (003F) and `!` (0021).

The spec also provides a `<Medial>` set, but Elixir does not include any character on this set. Therefore, the identifier rule has been simplified to consider this.

Elixir does not allow the use of ZWJ or ZWNJ in identifiers and therefore does not implement R1a. Bidirectional control characters are also not supported. R1b is guaranteed for backwards compatibility purposes.

#### Atoms

Unicode atoms in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` additionally includes the code point `_` (005F)
  * `<Continue>` additionally includes the code point `@` (0040)

Note atoms can also be quoted, which allows any characters, such as `:"hello elixir"`. All Elixir operators are also valid atoms, such as `:+`, `:@`, `:|>`, and others. The full description of valid atoms is available in the ["Atoms" section in the syntax reference](syntax-reference.md#atoms).

#### Variables, local calls, and remote calls

Variables in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` additionally includes the code point `_` (005F)
  * `<Start>` additionally excludes Lu (letter uppercase) and Lt (letter titlecase) characters

In set notation: `[\u{005F}\p{Ll}\p{Lm}\p{Lo}\p{Nl}\p{Other_ID_Start}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]`.

#### Aliases

Aliases in Elixir only allow ASCII characters, starting in uppercase, and no punctuation characters.

### R3. Pattern_White_Space and Pattern_Syntax Characters

Elixir supports only code points `\t` (0009), `\n` (000A), `\r` (000D) and `\s` (0020) as whitespace and therefore does not follow requirement R3. R3 requires a wider variety of whitespace and syntax characters to be supported.

### R4. Equivalent Normalized Identifiers

Identifiers in Elixir are case sensitive.

Elixir normalizes all atoms and variables to NFC form. Quoted-atoms and strings can, however, be in any form and are not verified by the parser.

In other words, the atom `:josé` can only be written with the code points `006A 006F 0073 00E9` or `006A 006F 0073 0065 0301`, but Elixir will rewrite it to the former (from Elixir 1.14). On the other hand, `:"josé"` may be written as `006A 006F 0073 00E9` or `006A 006F 0073 0065 0301` and its form will be retained, since it is written between quotes.

Choosing requirement R4 automatically excludes requirements R5, R6, and R7.

## Unicode Technical Standard #39

Elixir conforms to the clauses outlined in the [Unicode Technical Standard #39](https://unicode.org/reports/tr39/) on Security, version 15.0.

### C1. General Security Profile for Identifiers

Elixir will not allow tokenization of identifiers with codepoints in `\p{Identifier_Status=Restricted}`.

> An implementation following the General Security Profile does not permit any characters in \p{Identifier_Status=Restricted}, ...

For instance, the 'HANGUL FILLER' (`ㅤ`) character, which is often invisible, is an uncommon codepoint and will trigger this warning.

See the note below about additional normalizations, which can perform automatic replacement of some Restricted identifiers.

### C2. Confusable detection

Elixir will warn on identifiers that look the same, but aren't. Examples: in `а = a = 1`, the two 'a' characters are Cyrillic and Latin, and could be confused for each other; in `力 = カ = 1`, both are Japanese, but different codepoints, in different scripts of that writing system. Confusable identifiers can lead to hard-to-catch bugs (say, due to copy-pasted code) and can be unsafe, so we will warn about identifiers within a single file that could be confused with each other.

We use the means described in Section 4, 'Confusable Detection', with one noted modification

> Alternatively, it shall declare that it uses a modification, and provide a precise list of character mappings that are added to or removed from the provided ones.

Elixir will not warn on confusability for identifiers made up exclusively of characters in a-z, A-Z, 0-9, and _. This is because ASCII identifiers have existed for so long that the programming community has had their own means of dealing with confusability between identifiers like `l,1` or `O,0` (for instance, fonts designed for programming usually make it easy to differentiate between those characters).

### C3. Mixed Script Detection

Elixir will not allow tokenization of mixed-script identifiers unless it is via chunks separated by an underscore, like `http_сервер`. We use the means described in Section 5.1, Mixed-Script Detection, to determine if script mixing is occurring, with the modification documented in the section 'Additional Normalizations', below.

Examples: Elixir allows an identifiers like `幻ㄒㄧㄤ`, even though it includes characters from multiple 'scripts', because those scripts all 'resolve' to Japanese when applying the resolution rules from UTS 39 5.1. When mixing Latin and Japanese scripts, underscores are necessary, as in `:T_シャツ` (the Japanese word for 't-shirt' with an additional underscore separating the letter T).

Elixir does not allow code like `if аdmin, do: :ok, else: :err`, where the scriptset for the 'a' character is {Cyrillic} but all other characters have scriptsets of {Latin}. The scriptsets fail to resolve and a descriptive error is shown.

### C4, C5 (inapplicable)

'C4 - Restriction Level detection' conformance is not claimed and does not apply to identifiers in code; rather, it applies to classifying the level of safety of a given arbitrary string into one of 5 restriction levels.

'C5 - Mixed number detection' conformance is inapplicable as Elixir does not support Unicode numbers.

### Addition normalizations and documented UTS 39 modifications

As of Elixir 1.14, some codepoints in `\p{Identifier_Status=Restricted}` are *normalized* to other, unrestricted codepoints.

Initially this is only done to translate MICRO SIGN `µ` to Greek lowercase mu, `μ`.

This is not a modification of UTS39 clauses C1 (General Security Profile) or C2 (Confusability Detection); however, it is a documented modification of C3, 'Mixed-Script detection'.

Mixed-script detection is modified by these normalizations to the extent that the normalized codepoint is given the union of scriptsets from both characters.

  * For instance, in the example of MICRO => MU, Micro was a 'Common'-script character -- the same script given to the '_' underscore codepoint -- and thus the normalized character's scriptset will be {Greek, Common}. 'Common' intersects with all non-empty scriptsets, and thus the normalized character can be used in tokens written in any script without causing script mixing.

  * The code points normalized in this fashion are those that are in use in the community, and judged not likely to cause issues with unsafe script mixing. For instance, the MICRO or MU codepoint may be used in an atom or variable dealing with microseconds.
