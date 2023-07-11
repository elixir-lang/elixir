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
`in` `not in`                                  | Left
`\|>` `<<<` `>>>` `<<~` `~>>` `<~` `~>` `<~>`  | Left
`<` `>` `<=` `>=`                              | Left
`==` `!=` `=~` `===` `!==`                     | Left
`&&` `&&&` `and`                               | Left
`\|\|` `\|\|\|` `or`                           | Left
`=`                                            | Right
`&`                                            | Unary
`=>` (valid only inside `%{}`)                 | Right
`\|`                                           | Right
`::`                                           | Right
`when`                                         | Right
`<-` `\\`                                      | Left

## General operators

Elixir provides the following built-in operators:

  * [`+`](https://hexdocs.pm/elixir/main/Kernel.html#+/1) and [`-`](https://hexdocs.pm/elixir/main/Kernel.html#-/1) - unary positive/negative
  * [`+`](https://hexdocs.pm/elixir/main/Kernel.html#+/2), [`-`](https://hexdocs.pm/elixir/main/Kernel.html#-/2), [`*`](https://hexdocs.pm/elixir/main/Kernel.html#*/2), and [`/`](https://hexdocs.pm/elixir/main/Kernel.html#//2) - basic arithmetic operations
  * [`++`](https://hexdocs.pm/elixir/main/Kernel.html#++/2) and [`--`](https://hexdocs.pm/elixir/main/Kernel.html#--/2) - list concatenation and subtraction
  * [`and`](https://hexdocs.pm/elixir/main/Kernel.html#and/2) and [`&&`](https://hexdocs.pm/elixir/main/Kernel.html#&&/2) - strict and relaxed boolean "and"
  * [`or`](https://hexdocs.pm/elixir/main/Kernel.html#or/2) and [`||`](https://hexdocs.pm/elixir/main/Kernel.html#%7C%7C/2) - strict and relaxed boolean "or"
  * [`not`](https://hexdocs.pm/elixir/main/Kernel.html#not/1) and [`!`](https://hexdocs.pm/elixir/main/Kernel.html#!/1) - strict and relaxed boolean "not"
  * [`in`](https://hexdocs.pm/elixir/main/Kernel.html#in/2) and [`not in`](https://hexdocs.pm/elixir/main/Kernel.html#in/2) - membership
  * [`@`](https://hexdocs.pm/elixir/main/Kernel.html#@/1) - module attribute
  * [`..`](https://hexdocs.pm/elixir/main/Kernel.html#../0), [`..`](https://hexdocs.pm/elixir/main/Kernel.html#../2), and [`..//`](https://hexdocs.pm/elixir/main/Kernel.html#..///3) - range creation
  * [`<>`](https://hexdocs.pm/elixir/main/Kernel.html#%3C%3E/2) - binary concatenation
  * [`|>`](https://hexdocs.pm/elixir/main/Kernel.html#%7C%3E/2) - pipeline
  * [`=~`](https://hexdocs.pm/elixir/main/Kernel.html#=~/2) - text-based match

Many of those can be used in guards; consult the [list of allowed guard functions and operators](patterns-and-guards.md#list-of-allowed-functions-and-operators).

Additionally, there are a few other operators that Elixir parses but doesn't actually use.
See [Custom and overridden operators](#custom-and-overridden-operators) below for a list and for guidelines about their use.

Some other operators are special forms and cannot be overridden:

  * [`^`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#%5E/1) - pin operator
  * [`.`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#./2) - dot operator
  * [`=`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#=/2) - match operator
  * [`&`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#&/1) - capture operator
  * [`::`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#::/2) - type operator

Finally, these operators appear in the precedence table above but are only meaningful within certain constructs:

  * `=>` - see [`%{}`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#%25%7B%7D/1)
  * `when` - see [Guards](patterns-and-guards.md#guards)
  * `<-` - see [`for`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#for/1) and [`with`](https://hexdocs.pm/elixir/main/Kernel.SpecialForms.html#with/1)
  * `\\` - see [Default arguments](https://hexdocs.pm/elixir/main/Kernel.html#def/2-default-arguments)

## Comparison operators

Elixir provides the following built-in comparison operators (all of which can be used in guards):

  * [`==`](https://hexdocs.pm/elixir/main/Kernel.html#==/2) - equal to
  * [`===`](https://hexdocs.pm/elixir/main/Kernel.html#===/2) - strictly equal to
  * [`!=`](https://hexdocs.pm/elixir/main/Kernel.html#!=/2) - unequal to
  * [`!==`](https://hexdocs.pm/elixir/main/Kernel.html#!==/2) - strictly unequal to
  * [`<`](https://hexdocs.pm/elixir/main/Kernel.html#%3C/2) - less-than
  * [`>`](https://hexdocs.pm/elixir/main/Kernel.html#%3E/2) - greater-than
  * [`<=`](https://hexdocs.pm/elixir/main/Kernel.html#%3C=/2) - less-than or equal to
  * [`>=`](https://hexdocs.pm/elixir/main/Kernel.html#%3E=/2) - greater-than or equal to

The only difference between [`==`](https://hexdocs.pm/elixir/main/Kernel.html#==/2) and [`===`](https://hexdocs.pm/elixir/main/Kernel.html#===/2) is that [`===`](https://hexdocs.pm/elixir/main/Kernel.html#===/2) is strict when it comes to comparing integers and floats:

```elixir
iex> 1 == 1.0
true
iex> 1 === 1.0
false
```

[`!=`](https://hexdocs.pm/elixir/main/Kernel.html#!=/2) and [`!==`](https://hexdocs.pm/elixir/main/Kernel.html#!==/2) act as the negation of [`==`](https://hexdocs.pm/elixir/main/Kernel.html#==/2) and [`===`](https://hexdocs.pm/elixir/main/Kernel.html#===/2), respectively.

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

The following operators are used by the [`Bitwise`](https://hexdocs.pm/elixir/main/Bitwise.html) module when imported: [`&&&`](https://hexdocs.pm/elixir/main/Bitwise.html#&&&/2), [`<<<`](https://hexdocs.pm/elixir/main/Bitwise.html#%3C%3C%3C/2), [`>>>`](https://hexdocs.pm/elixir/main/Bitwise.html#%3E%3E%3E/2), and [`|||`](https://hexdocs.pm/elixir/main/Bitwise.html#%7C%7C%7C/2). See the documentation for [`Bitwise`](https://hexdocs.pm/elixir/main/Bitwise.html) for more information.

Note that the Elixir community generally discourages custom operators. They can be hard to read and even more to understand, as they don't have a descriptive name like functions do. That said, some specific cases or custom domain specific languages (DSLs) may justify these practices.

It is also possible to replace predefined operators, such as `+`, but doing so is extremely discouraged.
