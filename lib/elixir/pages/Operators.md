# Operators

This document covers operators in Elixir, how they are parsed, how they can be defined, and how they can be overridden.

## Operator precedence and associativity

The following is a list of all operators that Elixir is capable of parsing, ordered from higher to lower precedence, alongside their associativity:

Operator                                                                                 | Associativity
---------------------------------------------------------------------------------------- | -------------
`@`                                                                                      | Unary
`.`                                                                                      | Left to right
`+` `-` `!` `^` `not` `~~~`                                                              | Unary
`*` `/`                                                                                  | Left to right
`+` `-`                                                                                  | Left to right
`++` `--` `..` `<>`                                                                      | Right to left
`in`                                                                                     | Left to right
`\|>` `<<<` `>>>` `~>>` `<<~` `~>` `<~` `<~>` `<\|>`                                     | Left to right
`<` `>` `<=` `>=`                                                                        | Left to right
`==` `!=` `=~` `===` `!==`                                                               | Left to right
`&&` `&&&` `and`                                                                         | Left to right
`\|\|` `\|\|\|` `or`                                                                     | Left to right
`=`                                                                                      | Right to left
`=>`                                                                                     | Right to left
`\|`                                                                                     | Right to left
`::`                                                                                     | Right to left
`when`                                                                                   | Right to left
`<-`, `\\`                                                                               | Left to right
`&`                                                                                      | Unary

## Defining custom operators

Elixir is capable of parsing a predefined set of operators; this means that it's not possible to define new operators (like one could do in Haskell, for example). However, not all operators that Elixir can parse are *used* by Elixir: for example, `+` and `||` are used by Elixir for addition and boolean *or*, but `<~>` is not used (but valid).

To define an operator, you can use the usual `def*` constructs (`def`, `defp`, `defmacro`, and so on) but with a syntax similar to how the operator is used:

```elixir
defmodule MyOperators do
  # We define ~> to return the maximum of the given two numbers,
  # and <~ to return the minimum.

  def a ~> b, do: max(a, b)
  def a <~ b, do: min(a, b)
end
```

To use the newly defined operators, we **have to** import the module that defines them:

```elixir
iex> import MyOperators
iex> 1 ~> 2
2
iex> 1 <~ 2
1
```

The following is a table of all the operators that Elixir is capable of parsing, but that are not used by default:

  * `|`
  * `|||`
  * `&&&`
  * `<<<`
  * `>>>`
  * `~>>`
  * `<<~`
  * `~>`
  * `<~`
  * `<~>`
  * `<|>`
  * `^^^`
  * `~~~`

The following operators are used by the `Bitwise` module when imported: `&&&`, `^^^`, `<<<`, `>>>`, `|||`, `~~~`. See the documentation for `Bitwise` for more information.

## Redefining existing operators

The operators that Elixir uses (for example, `+`) can be defined by any module and used in place of the ones defined by Elixir, provided they're specifically not imported from `Kernel` (which is imported everywhere by default). For example:

```elixir
defmodule WrongMath do
  # Let's make math wrong by changing the meaning of +:
  def a + b, do: a - b
end
```

Now, we will get an error if we try to use this operator "out of the box":

```elixir
iex> import WrongMath
iex> 1 + 2
** (CompileError) iex:11: function +/2 imported from both WrongMath and Kernel, call is ambiguous
```

So, as mentioned above, we need to explicitly *not* import `+/2` from `Kernel`:

```elixir
iex> import WrongMath
iex> import Kernel, except: [+: 2]
iex> 1 + 2
-1
```

## Final note

While it's possible to defined unused operators (such as `<~>`) and to "override" predefined operators (such as `+`), the Elixir community generally discourages this. Custom-defined operators can be really hard to read and even more to understand, as they don't have a descriptive name like functions do. That said, some specific cases or custom domain specific languages (DSLs) may justify these practices.
