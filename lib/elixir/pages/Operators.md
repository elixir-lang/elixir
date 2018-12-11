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
`in` `not in`                                                                            | Left to right
`\|>` `<<<` `>>>` `~>>` `<<~` `~>` `<~` `<~>` `<\|>`                                     | Left to right
`<` `>` `<=` `>=`                                                                        | Left to right
`==` `!=` `=~` `===` `!==`                                                               | Left to right
`&&` `&&&` `and`                                                                         | Left to right
`\|\|` `\|\|\|` `or`                                                                     | Left to right
`&`                                                                                      | Unary
`=`                                                                                      | Right to left
`=>` (available only inside `%{}`)                                                       | Right to left
`\|`                                                                                     | Right to left
`::`                                                                                     | Right to left
`when`                                                                                   | Right to left
`<-` `\\`                                                                                | Left to right

## Comparison operators

Elixir provides the following built-in comparison operators:

  * [`==`](`Kernel.==/2`) - equality
  * [`===`](`Kernel.===/2`) - strict equality
  * [`!=`](`Kernel.!=/2`) - inequality
  * [`!==`](`Kernel.!==/2`) - strict inequality
  * [`>`](`Kernel.>/2`) - greater than
  * [`<`](`Kernel.</2`) - less than
  * [`>=`](`Kernel.>=/2`) - greater than or equal
  * [`<=`](`Kernel.<=/2`) - less than or equal

The only difference between [`==`](`Kernel.==/2`) and [`===`](`Kernel.===/2`) is that [`===`](`Kernel.===/2`) is stricter when it comes to comparing integers and floats:

```elixir
iex> 1 == 1.0
true
iex> 1 === 1.0
false
```

[`!=`](`Kernel.!=/2`) and [`!==`](`Kernel.!==/2`) act as the negation of [`==`](`Kernel.==/2`) and [`===`](`Kernel.===/2`), respectively.

### Term ordering

In Elixir, different data types can be compared using comparison operators:

```elixir
iex> 1 < :an_atom
true
```

The reason we can compare different data types is pragmatism. Sorting algorithms don’t need to worry about different data types in order to sort. For reference, the overall sorting order is defined below:

```
number < atom < reference < function < port < pid < tuple < map < list < bitstring
```

When comparing two numbers of different types (a number is either an integer or a float), a conversion to the type with greater precision will always occur, unless the comparison operator used is either `===` or `!==`. A float will be considered more precise than an integer, unless the float is greater/less than +/-9007199254740992.0, at which point all the significant figures of the float are to the left of the decimal point. This behavior exists so that the comparison of large numbers remains transitive.

The collection types are compared using the following rules:

* Tuples are compared by size then element by element.
* Maps are compared by size then by keys in ascending term order then by values in key order. In the specific case of maps' key ordering, integers are always considered to be less than floats.
* Lists are compared element by element.
* Bitstrings are compared byte by byte, incomplete bytes are compared bit by bit.

## Custom and overridden operators

### Defining custom operators

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

The following operators are used by the `Bitwise` module when imported: [`&&&`](`Bitwise.&&&/2`), [`^^^`](`Bitwise.^^^/2`), [`<<<`](`Bitwise.<<</2`), [`>>>`](`Bitwise.>>>/2`), [`|||`](`Bitwise.|||/2`), [`~~~`](`Bitwise.~~~/1`). See the documentation for `Bitwise` for more information.

### Redefining existing operators

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

### Final note

While it's possible to define unused operators (such as `<~>`) and to "override" predefined operators (such as `+`), the Elixir community generally discourages this. Custom-defined operators can be really hard to read and even more to understand, as they don't have a descriptive name like functions do. That said, some specific cases or custom domain specific languages (DSLs) may justify these practices.
