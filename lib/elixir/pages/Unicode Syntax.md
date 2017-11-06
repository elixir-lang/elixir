# Unicode Syntax

Elixir supports Unicode throughout the language.

Quoted identifiers, such as strings (`"olá"`) and charlists (`'olá'`), support Unicode since Elixir v1.0. Strings are UTF-8 encoded. Charlists are lists of Unicode codepoints. In such cases, the contents are kept as written by developers, without any transformation.

Elixir also supports Unicode in identifiers since Elixir v1.5, as defined in the [Unicode Annex #31](http://unicode.org/reports/tr31/). This is the focus of this document.

## Version

The Unicode Annex specify a list of requirements that may or may not be implemented by programming languages. This document lists all of the requirements implemented by Elixir, refered as R1, R6 and so on.

To check the Unicode version of your current Elixir installation, please run `String.Unicode.version()`.

## R1. Default Identifiers

Elixir identifiers are identified as:

    <Identifier> := <Start> <Continue>* <Ending>?

where `<Start>` is:

> characters derived from the Unicode General Category of uppercase letters, lowercase letters, titlecase letters, modifier letters, other letters, letter numbers, plus `Other_ID_Start`, minus `Pattern_Syntax` and `Pattern_White_Space` code points
>
> In set notation: `[[:L:][:Nl:][:Other_ID_Start:]--[:Pattern_Syntax:]--[:Pattern_White_Space:]]`

and `<Continue>` is:

> ID_Start characters, plus characters having the Unicode General Category of nonspacing marks, spacing combining marks, decimal number, connector punctuation, plus `Other_ID_Continue`, minus `Pattern_Syntax` and `Pattern_White_Space` code points.
>
> In set notation: `[[:ID_Start:][:Mn:][:Mc:][:Nd:][:Pc:][:Other_ID_Continue:]--[:Pattern_Syntax:]--[:Pattern_White_Space:]]`

`<Ending>` is an addition specific to Elixir that includes the codepoints ? (003F) and ! (0021).

Elixir also implements requirements R1a for security and R1b for backwards compatibility.

### Atoms

Atoms in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` includes the codepoint _ (005F)
  * `<Continue>` includes the codepoint @ (0040)

### Variables

Variables in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` includes the codepoint _ (005F)
  * `<Start>` must not include Lu (letter uppercase) and Lt (letter titlecase) characters
  * `<Continue>` includes Lu (letter uppercase) and Lt (letter titlecase) characters

## R3. Pattern_White_Space and Pattern_Syntax Characters

Elixir supports only codepoints \t (0009), \n (000A), \r (000D) and \s (0020) as whitespace and therefore does not follow requirement R3. R3 requires a wider variety of whitespace and syntax characters to be supported.

## R6. Filtered Normalized Identifiers

Identifiers in Elixir are case sensitive.

Elixir requires all atoms and variables to be in NFC form. Any other form will fail with a relevant error message. Quoted-atoms and variables can, however, be in any form and are not verified by the parser.

In other words, the atom `:josé` can only be written with the codepoints 006A 006F 0073 00E9. Using another normalization form will lead to a tokenizer error. On the other hand, `:"josé"` may be written as 006A 006F 0073 00E9 or 006A 006F 0073 0065 0301, since it is written in quotes.

Choosing requirement R6 automatically excludes requirements R4, R5 and R7.
