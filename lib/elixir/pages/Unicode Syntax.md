# Unicode Syntax

Elixir supports Unicode throughout the language.

Quoted identifiers, such as strings (`"olá"`) and charlists (`'olá'`), support Unicode since Elixir v1.0. Strings are UTF-8 encoded. Charlists are lists of Unicode code points. In such cases, the contents are kept as written by developers, without any transformation.

Elixir also supports Unicode in identifiers since Elixir v1.5, as defined in the [Unicode Annex #31](http://unicode.org/reports/tr31/). The focus of this document is to describe how Elixir implements the requirements outlined in the Unicode Annex. These requirements are referred to as R1, R6 and so on.

To check the Unicode version of your current Elixir installation, run `String.Unicode.version()`.

## R1. Default Identifiers

The general Elixir identifier rule is specified as:

    <Identifier> := <Start> <Continue>* <Ending>?

where `<Start>` uses the same categories as the spec but restricts them to the NFC form (see R6):

> characters derived from the Unicode General Category of uppercase letters, lowercase letters, titlecase letters, modifier letters, other letters, letter numbers, plus `Other_ID_Start`, minus `Pattern_Syntax` and `Pattern_White_Space` code points
>
> In set notation: `[\p{L}\p{Nl}\p{Other_ID_Start}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]`

and `<Continue>` uses the same categories as the spec but restricts them to the NFC form (see R6):

> ID_Start characters, plus characters having the Unicode General Category of nonspacing marks, spacing combining marks, decimal number, connector punctuation, plus `Other_ID_Continue`, minus `Pattern_Syntax` and `Pattern_White_Space` code points.
>
> In set notation: `[\p{ID_Start}\p{Mn}\p{Mc}\p{Nd}\p{Pc}\p{Other_ID_Continue}-\p{Pattern_Syntax}-\p{Pattern_White_Space}]`

`<Ending>` is an addition specific to Elixir that includes only the code points `?` (003F) and `!` (0021).

The spec also provides a `<Medial>` set but Elixir does not include any character on this set. Therefore the identifier rule has been simplified to consider this.

Elixir does not allow the use of ZWJ or ZWNJ in identifiers and therefore does not implement R1a. R1b is guaranteed for backwards compatibility purposes.

### Atoms

Unicode atoms in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` includes the code point `_` (005F)
  * `<Continue>` includes the code point `@` (0040)

> Note that all Elixir operators are also valid atoms. Therefore `:+`, `:@`, `:|>`, and others are all valid atoms. The full description of valid atoms is available in the Syntax Reference, this document covers only the rules for identifier-based atoms.

### Variables

Variables in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` includes the code point `_` (005F)
  * `<Start>` must not include Lu (letter uppercase) and Lt (letter titlecase) characters

## R3. Pattern_White_Space and Pattern_Syntax Characters

Elixir supports only code points `\t` (0009), `\n` (000A), `\r` (000D) and `\s` (0020) as whitespace and therefore does not follow requirement R3. R3 requires a wider variety of whitespace and syntax characters to be supported.

## R6. Filtered Normalized Identifiers

Identifiers in Elixir are case sensitive.

Elixir requires all atoms and variables to be in NFC form. Any other form will fail with a relevant error message. Quoted-atoms and strings can, however, be in any form and are not verified by the parser.

In other words, the atom `:josé` can only be written with the code points `006A 006F 0073 00E9`. Using another normalization form will lead to a tokenizer error. On the other hand, `:"josé"` may be written as `006A 006F 0073 00E9` or `006A 006F 0073 0065 0301`, since it is written between quotes.

Choosing requirement R6 automatically excludes requirements R4, R5 and R7.
