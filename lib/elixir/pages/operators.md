# Operators

This document covers operators in Elixir, how they are parsed, how they can be defined, and how they can be overridden.

## Operator precedence and associativity

The following is a list of all operators that Elixir is capable of parsing, ordered from higher to lower precedence, alongside their associativity:

Operator                                              | Associativity
----------------------------------------------------- | -------------
`@`                                                   | Unary
`.`                                                   | Left
`+` `-` `!` `^` `not` `~~~`                           | Unary
`*` `/`                                               | Left
`+` `-`                                               | Left
`++` `--` `+++` `---` `..` `<>`                       | Right
`in` `not in`                                         | Left
`\|>` `<<<` `>>>` `<<~` `~>>` `<~` `~>` `<~>` `<\|>`  | Left
`<` `>` `<=` `>=`                                     | Left
`==` `!=` `=~` `===` `!==`                            | Left
`&&` `&&&` `and`                                      | Left
`\|\|` `\|\|\|` `or`                                  | Left
`=`                                                   | Right
`&`                                                   | Unary
`=>` (valid only inside `%{}`)                        | Right
`\|`                                                  | Right
`::`                                                  | Right
`when`                                                | Right
`<-` `\\`                                             | Left

## General operators

Elixir provides the following built-in operators that are defined as functions that can be overridden:

  * [`+`](`+/1`) and [`-`](`-/1`) - unary positive/negative
  * [`+`](`+/2`), [`-`](`-/2`), [`*`](`*/2`), and [`/`](`//2`) - basic arithmetic operations
  * [`++`](`++/2`) and [`--`](`--/2`) - list concatenation and subtraction
  * [`and`](`and/2`) and [`&&`](`&&/2`) - strict and relaxed boolean "and"
  * [`or`](`or/2`) and [`||`](`||/2`) - strict and relaxed boolean "or"
  * [`not`](`not/1`) and [`!`](`!/1`) - strict and relaxed boolean "not"
  * [`in`](`in/2`) and [`not in`](`in/2`) - membership
  * [`@`](`@/1`) - module attribute
  * [`..`](`../2`) - range creation
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
  * [`::`](`Kernel.SpecialForms.::/2`) - type operator

Finally, these operators appear in the precedence table above but are only meaningful within certain constructs:

  * `=>` - see [`%{}`](`%{}/1`)
  * `when` - see [Guards](patterns-and-guards.md#guards)
  * `<-` - see [`for`](`for/1`) and [`with`](`with/1`)
  * `\\` - see [Default arguments](Kernel.html#def/2-default-arguments)

## Comparison operators

Elixir provides the following built-in comparison operators (all of which can be used in guards):

  * [`==`](`==/2`) - equal to
  * [`===`](`===/2`) - strictly equal to
  * [`!=`](`!=/2`) - inequal to
  * [`!==`](`!==/2`) - strictly inequal to
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

### Term ordering

In Elixir, different data types can be compared using comparison operators:

```elixir
iex> 1 < :an_atom
true
```

The reason we can compare different data types is pragmatism. Sorting algorithms don't need to worry about different data types in order to sort. For reference, the overall sorting order is defined below:

```
number < atom < reference < function < port < pid < tuple < map < list < bitstring
```

When comparing two numbers of different types (a number being either an integer or a float), a conversion to the type with greater precision will always occur, unless the comparison operator used is either [`===`](`===/2`) or [`!==`](`!==/2`). A float will be considered more precise than an integer, unless the float is greater/less than +/-9007199254740992.0 respectively, at which point all the significant figures of the float are to the left of the decimal point. This behavior exists so that the comparison of large numbers remains transitive.

The collection types are compared using the following rules:

* Tuples are compared by size, then element by element.
* Maps are compared by size, then by keys in ascending term order, then by values in key order. In the specific case of maps' key ordering, integers are always considered to be less than floats.
* Lists are compared element by element.
* Bitstrings are compared byte by byte, incomplete bytes are compared bit by bit.
* Atoms are compared using their string value, codepoint by codepoint.

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

  * `|||`
  * `&&&`
  * `<<<`
  * `>>>`
  * `<<~`
  * `~>>`
  * `<~`
  * `~>`
  * `<~>`
  * `<|>`
  * `+++`
  * `---`
  * `~~~`

The following operators are used by the `Bitwise` module when imported: [`&&&`](`Bitwise.&&&/2`), [`<<<`](`Bitwise.<<</2`), [`>>>`](`Bitwise.>>>/2`), [`|||`](`Bitwise.|||/2`), [`~~~`](`Bitwise.~~~/1`). See the documentation for `Bitwise` for more information.

Note the Elixir community generally discourages custom operators. They can be hard to read and even more to understand, as they don't have a descriptive name like functions do. That said, some specific cases or custom domain specific languages (DSLs) may justify these practices.

It is also possible replace predefined operators, such as `+`, but doing so is extremely discouraged.
