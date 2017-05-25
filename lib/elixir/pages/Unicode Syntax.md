# Unicode Syntax

Elixir implements [Unicode Annex #31](http://unicode.org/reports/tr31/) for non-quoted atoms and variables as specified in this document.

## Version

To check the Unicode version of your current Elixir installation please run `String.Unicode.version()`.

The changes in this document were included to Elixir v1.5 and require OTP 20+.

## R1. Default Identifiers

Elixir identifiers are identified as:

    <Identifier> := <Start> <Continue>* <Ending>?

where `<Start>` is:

> characters derived from the Unicode General Category of uppercase letters, lowercase letters, titlecase letters, modifier letters, other letters, letter numbers, plus Other_ID_Start, minus Pattern_Syntax and Pattern_White_Space code points
>
> In set notation: [[:L:][:Nl:][:Other_ID_Start:]--[:Pattern_Syntax:]--[:Pattern_White_Space:]]

and `<Continue>` is:

> ID_Start characters, plus characters having the Unicode General Category of nonspacing marks, spacing combining marks, decimal number, connector punctuation, plus Other_ID_Continue, minus Pattern_Syntax and Pattern_White_Space code points.
>
> In set notation: [[:ID_Start:][:Mn:][:Mc:][:Nd:][:Pc:][:Other_ID_Continue:]--[:Pattern_Syntax:]--[:Pattern_White_Space:]]

`<Ending>` is an addition specific to Elixir that includes the codepoints ? (003F) and ! (0021).

Elixir does not implement requirement R1a. It does implement requirement R1b.

### Atoms

Atoms in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` includes the codepoint _ (005F)
  * `<Continue>` includes the codepoint @ (0040)

### Variables

Atoms in Elixir follow the identifier rule above with the following modifications:

  * `<Start>` includes the codepoint _ (005F)
  * `<Start>` must not include Lu (letter upcase) and Lt (letter titlecase) characters
  * `<Continue>` includes Lu (letter upcase) and Lt (letter titlecase) characters

## R6. Filtered Normalized Identifiers

Identifiers in Elixir are case sensitive.

Elixir requires all atoms and variables to be in NFC form. Any other form will fail with a relevant error message. Quoted-atoms and variables can, however, be in any form and are not verified by the parser.

In other words, the atom `:josé` can only be written with the codepoints 006A 006F 0073 00E9. On the other hand, `:"josé"` may be written as 006A 006F 0073 00E9 or 006A 006F 0073 0065 0301.

## Other considerations

It is worth noting that Elixir supports only codepoints \t (0009), \n (000A), \r (000D) and \s (0020) as whitespace and therefore does not follow requirement R3.
