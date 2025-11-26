<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Operators reference

This document is a complete reference of operators in Elixir, how they are parsed, how they can be defined, and how they can be overridden.

## Operator precedence and associativity

The following is a list of all operators that Elixir is capable of parsing, ordered from higher to lower precedence, alongside their associativity:

Operator                                       | Associativity
---------------------------------------------- | -------------
`@`                                            | Unary
`.`                                            | Left
`+` `-` `!` `^` `not`                          | Unary
`**`                                           | Left
`*` `/`                                        | Left
`+` `-`                                        | Left
`++` `--` `+++` `---` `..` `<>`                | Right
`//` (valid only inside `..//`)                | Right
`in` `not in`                                  | Left
`\|>` `<<<` `>>>` `<<~` `~>>` `<~` `~>` `<~>`  | Left
`<` `>` `<=` `>=`                              | Left
`==` `!=` `=~` `===` `!==`                     | Left
`&&` `&&&` `and`                               | Left
`\|\|` `\|\|\|` `or`                           | Left
`=`                                            | Right
`&`, `...`                                     | Unary
`\|`                                           | Right
`::`                                           | Right
`when`                                         | Right
`<-` `\\`                                      | Left
`=>` (valid only inside `%{}`)                 | None

Elixir also has two ternary operators:

Operator
------------------------------
`first..last//step`                
`%{map | key => value, ...}`

## General operators

Elixir provides the following built-in operators:

  * [`+`](`+/1`) and [`-`](`-/1`) - unary positive/negative
  * [`+`](`+/2`), [`-`](`-/2`), [`*`](`*/2`), and [`/`](`//2`) - basic arithmetic operations
  * [`++`](`++/2`) and [`--`](`--/2`) - list concatenation and subtraction
  * [`and`](`and/2`) and [`&&`](`&&/2`) - strict and relaxed boolean "and"
  * [`or`](`or/2`) and [`||`](`||/2`) - strict and relaxed boolean "or"
  * [`not`](`not/1`) and [`!`](`!/1`) - strict and relaxed boolean "not"
  * [`in`](`in/2`) and [`not in`](`in/2`) - membership
  * [`@`](`@/1`) - module attribute
  * [`..`](`../0`), [`..`](`../2`), and [`..//`](`..///3`) - range creation
  * [`<>`](`<>/2`) - binary concatenation
  * [`|>`](`|>/2`) - pipeline
  * [`=~`](`=~/2`) - text-based match

Many of those can be used in guards; consult the [list of allowed guard functions and operators](patterns-and-guards.md#list-of-allowed-functions-and-operators).

Additionally, there are a few other operators that Elixir parses but doesn't actually use.
See [Custom and overridden operators](#custom-and-overridden-operators) below for a list and for guidelines about their use.

Some other operators are special forms and cannot be overridden:

  * [`^`](`^/1`) - pin operator
  * [`.`](`./2`) - dot operator
  * [`=`](`=/2`) - match operator
  * [`&`](`&/1`) - capture operator
  * [`::`](`::/2`) - type operator

Finally, these operators appear in the precedence table above but are only meaningful within certain constructs:

  * `=>` - see [`%{}`](`%{}/1`)
  * `when` - see [Guards](patterns-and-guards.md#guards)
  * `<-` - see [`for`](`for/1`) and [`with`](`with/1`)
  * `\\` - see [Default arguments](`Kernel#def/2-default-arguments`)

## Comparison operators

Elixir provides the following built-in comparison operators (all of which can be used in guards):

  * [`==`](`==/2`) - equal to
  * [`===`](`===/2`) - strictly equal to
  * [`!=`](`!=/2`) - not equal to
  * [`!==`](`!==/2`) - strictly not equal to
  * [`<`](`</2`) - less-than
  * [`>`](`>/2`) - greater-than
  * [`<=`](`<=/2`) - less-than or equal to
  * [`>=`](`>=/2`) - greater-than or equal to

The only difference between [`==`](`==/2`) and [`===`](`===/2`) is that [`===`](`===/2`) is strict when it comes to comparing integers and floats:

```elixir
iex> 1 == 1.0
true
iex> 1 === 1.0
false
```

[`!=`](`!=/2`) and [`!==`](`!==/2`) act as the negation of [`==`](`==/2`) and [`===`](`===/2`), respectively.

## Custom and overridden operators

### Defining custom operators

Elixir is capable of parsing a predefined set of operators. It's not possible to define new operators (as supported by some languages). However, not all operators that Elixir can parse are *used* by Elixir: for example, `+` and `||` are used by Elixir for addition and boolean *or*, but `<~>` is not used (but valid).

To define an operator, you can use the usual `def*` constructs (`def`, `defp`, `defmacro`, and so on) but with a syntax similar to how the operator is used:

```elixir
defmodule MyOperators do
  # We define ~> to return the maximum of the given two numbers,
  # and <~ to return the minimum.

  def a ~> b, do: max(a, b)
  def a <~ b, do: min(a, b)
end
```

To use the newly defined operators, you **have to** import the module that defines them:

```elixir
iex> import MyOperators
iex> 1 ~> 2
2
iex> 1 <~ 2
1
```

The following is a table of all the operators that Elixir is capable of parsing, but that are not used by default:

  * `|||`
  * `&&&`
  * `<<<`
  * `>>>`
  * `<<~`
  * `~>>`
  * `<~`
  * `~>`
  * `<~>`
  * `+++`
  * `---`
  * `...`

The following operators are used by the `Bitwise` module when imported: [`&&&`](`Bitwise.&&&/2`), [`<<<`](`Bitwise.<<</2`), [`>>>`](`Bitwise.>>>/2`), and [`|||`](`Bitwise.|||/2`). See the `Bitwise` documentation for more information.

Note that the Elixir community generally discourages custom operators. They can be hard to read and even more to understand, as they don't have a descriptive name like functions do. That said, some specific cases or custom domain specific languages (DSLs) may justify these practices.

It is also possible to replace predefined operators, such as `+`, but doing so is extremely discouraged.
