<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Quote and unquote

This guide aims to introduce the meta-programming techniques available in Elixir. The ability to represent an Elixir program by its own data structures is at the heart of meta-programming. This chapter starts by exploring those structures and the associated `quote/2` and `unquote/1` constructs, so we can take a look at macros in the next guide, and finally build our own domain specific language.

## Quoting

The building block of an Elixir program is a tuple with three elements. For example, the function call `sum(1, 2, 3)` is represented internally as:

```elixir
{:sum, [], [1, 2, 3]}
```

You can get the representation of any expression by using the `quote/2` macro:

```elixir
iex> quote do: sum(1, 2, 3)
{:sum, [], [1, 2, 3]}
```

The first element is the function name, the second is a keyword list containing metadata, and the third is the arguments list.

Operators are also represented as such tuples:

```elixir
iex> quote do: 1 + 2
{:+, [context: Elixir, import: Kernel], [1, 2]}
```

Even a map is represented as a call to `%{}`:

```elixir
iex> quote do: %{1 => 2}
{:%{}, [], [{1, 2}]}
```

Variables are represented using such triplets, with the difference that the last element is an atom, instead of a list:

```elixir
iex> quote do: x
{:x, [], Elixir}
```

When quoting more complex expressions, we can see that the code is represented in such tuples, which are often nested inside each other in a structure resembling a tree. Many languages would call such representations an [*Abstract Syntax Tree*](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST). Elixir calls them *quoted expressions*:

```elixir
iex> quote do: sum(1, 2 + 3, 4)
{:sum, [], [1, {:+, [context: Elixir, import: Kernel], [2, 3]}, 4]}
```

Sometimes, when working with quoted expressions, it may be useful to get the textual code representation back. This can be done with `Macro.to_string/1`:

```elixir
iex> Macro.to_string(quote do: sum(1, 2 + 3, 4))
"sum(1, 2 + 3, 4)"
```

In general, the tuples above are structured according to the following format:

```elixir
{atom | tuple, list, list | atom}
```

  * The first element is an atom or another tuple in the same representation;
  * The second element is a keyword list containing metadata, like numbers and contexts;
  * The third element is either a list of arguments for the function call or an atom. When this element is an atom, it means the tuple represents a variable.

Besides the tuple defined above, there are five Elixir literals that, when quoted, return themselves (and not a tuple). They are:

```elixir
:sum         #=> Atoms
1.0          #=> Numbers
[1, 2]       #=> Lists
"strings"    #=> Strings
{key, value} #=> Tuples with two elements
```

Most Elixir code has a straight-forward translation to its underlying quoted expression. We recommend you try out different code samples and see what the results are. For example, what does `String.upcase("foo")` expand to? We have also learned that `if(true, do: :this, else: :that)` is the same as `if true do :this else :that end`. How does this affirmation hold with quoted expressions?

## Unquoting

Quoting is about retrieving the inner representation of some particular chunk of code. However, sometimes it may be necessary to inject some other particular chunk of code inside the representation we want to retrieve.

For example, imagine you have a variable called `number` which contains the number you want to inject inside a quoted expression.

```elixir
iex> number = 13
iex> Macro.to_string(quote do: 11 + number)
"11 + number"
```

That's not what we wanted, since the value of the `number` variable has not been injected and `number` has been quoted in the expression. In order to inject the *value* of the `number` variable, `unquote/1` has to be used inside the quoted representation:

```elixir
iex> number = 13
iex> Macro.to_string(quote do: 11 + unquote(number))
"11 + 13"
```

`unquote/1` can even be used to inject function names:

```elixir
iex> fun = :hello
iex> Macro.to_string(quote do: unquote(fun)(:world))
"hello(:world)"
```

In some cases, it may be necessary to inject many values inside a list. For example, imagine you have a list containing `[1, 2, 6]`, and we want to inject `[3, 4, 5]` into it. Using `unquote/1` won't yield the desired result:

```elixir
iex> inner = [3, 4, 5]
iex> Macro.to_string(quote do: [1, 2, unquote(inner), 6])
"[1, 2, [3, 4, 5], 6]"
```

That's when `unquote_splicing/1` comes in handy:

```elixir
iex> inner = [3, 4, 5]
iex> Macro.to_string(quote do: [1, 2, unquote_splicing(inner), 6])
"[1, 2, 3, 4, 5, 6]"
```

Unquoting is very useful when working with macros. When writing macros, developers are able to receive code chunks and inject them inside other code chunks, which can be used to transform code or write code that generates code during compilation.

## Escaping

As we saw at the beginning of this chapter, only some values are valid quoted expressions in Elixir. For example, a map is not a valid quoted expression. Neither is a tuple with four elements. However, such values *can* be expressed as a quoted expression:

```elixir
iex> quote do: %{1 => 2}
{:%{}, [], [{1, 2}]}
```

In some cases, you may need to inject such *values* into *quoted expressions*. To do that, we need to first escape those values into quoted expressions with the help of `Macro.escape/1`:

```elixir
iex> map = %{hello: :world}
iex> Macro.escape(map)
{:%{}, [], [hello: :world]}
```

Macros receive quoted expressions and must return quoted expressions. However, sometimes during the execution of a macro, you may need to work with values and making a distinction between values and quoted expressions will be required.

In other words, it is important to make a distinction between a regular Elixir value (like a list, a map, a process, a reference, and so on) and a quoted expression. Some values, such as integers, atoms, and strings, have a quoted expression equal to the value itself. Other values, like maps, need to be explicitly converted. Finally, values like functions and references cannot be converted to a quoted expression at all.

When working with macros and code that generates code, check out the documentation for the `Macro` module, which contains many functions to work with Elixir's AST.

In this introduction, we have laid the groundwork to finally write our first macro. You can check that out in the [next guide](macros.md).
