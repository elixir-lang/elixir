<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Syntax reference

Elixir syntax was designed to have a straightforward conversion to an abstract syntax tree (AST). This means the Elixir syntax is mostly uniform with a handful of "syntax sugar" constructs to reduce the noise in common Elixir idioms.

This document covers all of Elixir syntax constructs as a reference and then discuss their exact AST representation.

## Reserved words

These are the reserved words in the Elixir language. They are detailed throughout this guide but summed up here for convenience:

  * `true`, `false`, `nil` - used as atoms
  * `when`, `and`, `or`, `not`, `in` - used as operators
  * `fn` - used for anonymous function definitions
  * `do`, `end`, `catch`, `rescue`, `after`, `else` - used in do-end blocks

## Data types

### Numbers

Integers (`1234`) and floats (`123.4`) in Elixir are represented as a sequence of digits that may be separated by underscore for readability purposes, such as `1_000_000`. Integers never contain a dot (`.`) in their representation. Floats contain a dot and at least one other digit after the dot. Floats also support the scientific notation, such as `123.4e10` or `123.4E10`.

### Atoms

Unquoted atoms start with a colon (`:`) which must be immediately followed by a Unicode letter or an underscore. The atom may continue using a sequence of Unicode letters, numbers, underscores, and `@`. Atoms may end in `!` or `?`. Valid unquoted atoms are: `:ok`, `:ISO8601`, and `:integer?`.

If the colon is immediately followed by a pair of double- or single-quotes surrounding the atom name, the atom is considered quoted. In contrast with an unquoted atom, this one can be made of any Unicode character (not only letters), such as `:'🌢 Elixir'`, `:"++olá++"`, and `:"123"`.

Quoted and unquoted atoms with the same name are considered equivalent, so `:atom`, `:"atom"`, and `:'atom'` represent the same atom. The only catch is that the compiler will warn when quotes are used in atoms that do not need to be quoted.

All operators in Elixir are also valid atoms. Valid examples are `:foo`, `:FOO`, `:foo_42`, `:foo@bar`, and `:++`. Invalid examples are `:@foo` (`@` is not allowed at start), `:123` (numbers are not allowed at start), and `:(*)` (not a valid operator).

`true`, `false`, and `nil` are reserved words that are represented by the atoms `:true`, `:false` and `:nil` respectively.

To learn more about all Unicode characters allowed in atom, see the [Unicode syntax](unicode-syntax.md) document.

### Strings

Single-line strings in Elixir are written between double-quotes, such as `"foo"`. Any double-quote inside the string must be escaped with `\ `. Strings support Unicode characters and are stored as UTF-8 encoded binaries.

Multi-line strings in Elixir are called heredocs. They are written with three double-quotes, and can have unescaped quotes within them. The resulting string will end with a newline. The indentation of the last `"""` is used to strip indentation from the inner string. For example:

```elixir
iex> test = """
...>     this
...>     is
...>     a
...>     test
...> """
"    this\n    is\n    a\n    test\n"
iex> test = """
...>     This
...>     Is
...>     A
...>     Test
...>     """
"This\nIs\nA\nTest\n"
```

Strings are always represented as themselves in the AST.

### Charlists

Charlists are lists of non-negative integers where each integer represents a Unicode code point.

```elixir
iex(6)> 'abc' === [97, 98, 99]
true
```

Charlists are written in single-quotes, such as `'foo'`. Any single-quote inside the string must be escaped with `\ `.
Multi-line charlists are written with three single-quotes (`'''`), the same way multi-line strings are.
However, this syntax is deprecated in favor of the charlist sigil `~c`.

Charlists are always represented as themselves in the AST.

For more in-depth information, please read the "Charlists" section in the `List` module.

### Lists, tuples and binaries

Data structures such as lists, tuples, and binaries are marked respectively by the delimiters `[...]`, `{...}`, and `<<...>>`. Each element is separated by comma. A trailing comma is also allowed, such as in `[1, 2, 3,]`.

### Maps and keyword lists

Maps use the `%{...}` notation and each key-value is given by pairs marked with `=>`, such as `%{"hello" => 1, 2 => "world"}`.

Both keyword lists (list of two-element tuples where the first element is an atom) and maps with atom keys support a keyword notation where the colon character `:` is moved to the end of the atom. `%{hello: "world"}` is equivalent to `%{:hello => "world"}` and `[foo: :bar]` is equivalent to `[{:foo, :bar}]`. We discuss keywords in later sections.

### Structs

Structs built on the map syntax by passing the struct name between `%` and `{`. For example, `%User{...}`.

## Expressions

### Variables

Variables in Elixir must start with an underscore or a Unicode letter that is not in uppercase or titlecase. The variable may continue using a sequence of Unicode letters, numbers, and underscores. Variables may end in `?` or `!`. To learn more about all Unicode characters allowed in variables, see the [Unicode syntax](unicode-syntax.md) document.

[Elixir's naming conventions](naming-conventions.md) recommend variables to be in `snake_case` format.

### Non-qualified calls (local calls)

Non-qualified calls, such as `add(1, 2)`, must start with characters and then follow the same rules as variables, which are optionally followed by parentheses, and then arguments.

Parentheses are required for zero-arity calls (i.e. calls without arguments), to avoid ambiguity with variables. If parentheses are used, they must immediately follow the function name *without spaces*. For example, `add (1, 2)` is a syntax error, since `(1, 2)` is treated as an invalid block which is attempted to be given as a single argument to `add`.

[Elixir's naming conventions](naming-conventions.md) recommend calls to be in `snake_case` format.

### Operators

As many programming languages, Elixir also support operators as non-qualified calls with their precedence and associativity rules. Constructs such as `=`, `when`, `&` and `@` are simply treated as operators. See [the Operators page](operators.md) for a full reference.

### Qualified calls (remote calls)

Qualified calls, such as `Math.add(1, 2)`, must start with characters and then follow the same rules as variables, which are optionally followed by parentheses, and then arguments. Qualified calls also support operators, such as `Kernel.+(1, 2)`. Elixir also allows the function name to be written between double- or single-quotes, allowing any character in between the quotes, such as `Math."++add++"(1, 2)`.

Similar to non-qualified calls, parentheses have different meaning for zero-arity calls (i.e. calls without arguments). If parentheses are used, such as `mod.fun()`, it means a function call. If parenthesis are skipped, such as `map.field`, it means accessing a field of a map.

[Elixir's naming conventions](naming-conventions.md) recommend calls to be in `snake_case` format.

### Aliases

Aliases are constructs that expand to atoms at compile-time. The alias `String` expands to the atom `:"Elixir.String"`. Aliases must start with an ASCII uppercase character which may be followed by any ASCII letter, number, or underscore. Non-ASCII characters are not supported in aliases.

Multiple aliases can be joined with `.`, such as `MyApp.String`, and it expands to the atom `:"Elixir.MyApp.String"`. The dot is effectively part of the name but it can also be used for composition. If you define `alias MyApp.Example, as: Example` in your code, then `Example` will always expand to `:"Elixir.MyApp.Example"` and `Example.String` will expand to `:"Elixir.MyApp.Example.String"`.

[Elixir's naming conventions](naming-conventions.md) recommend aliases to be in `CamelCase` format.

### Module attributes

Module attributes are module-specific storage and are written as the composition of the unary operator `@` with variables and local calls. For example, to write to a module attribute named `foo`, use `@foo "value"`, and use `@foo` to read from it. Given module attributes are written using existing constructs, they follow the same rules above defined for operators, variables, and local calls.

### Blocks

Blocks are multiple Elixir expressions separated by newlines or semi-colons. A new block may be created at any moment by using parentheses.

### Left to right arrow

The left to right arrow (`->`) is used to establish a relationship between left and right, commonly referred as clauses. The left side may have zero, one, or more arguments; the right side is zero, one, or more expressions separated by new line. The `->` may appear one or more times between one of the following terminators: `do`-`end`, `fn`-`end` or `(`-`)`. When `->` is used, only other clauses are allowed between those terminators. Mixing clauses and regular expressions is invalid syntax.

It is seen on `case` and `cond` constructs between `do` and `end`:

```elixir
case 1 do
  2 -> 3
  4 -> 5
end

cond do
  true -> false
end
```

Seen in typespecs between `(` and `)`:

```elixir
(integer(), boolean() -> integer())
```

It is also used between `fn` and `end` for building anonymous functions:

```elixir
fn
  x, y -> x + y
end
```

### Sigils

Sigils start with `~` and are followed by one lowercase letter or by one or more uppercase letters, immediately followed by one of the following pairs:

  * `(` and `)`
  * `{` and `}`
  * `[` and `]`
  * `<` and `>`
  * `"` and `"`
  * `'` and `'`
  * `|` and `|`
  * `/` and `/`

After closing the pair, zero or more ASCII letters and digits can be given as a modifier. Sigils are expressed as non-qualified calls prefixed with `sigil_` where the first argument is the sigil contents as a string and the second argument is a list of integers as modifiers:

If the sigil letter is in uppercase, no interpolation is allowed in the sigil, otherwise its contents may be dynamic. Compare the results of the sigils below for more information:

```elixir
~s/f#{"o"}o/
~S/f#{"o"}o/
```

Sigils are useful to encode text with their own escaping rules, such as regular expressions, datetimes, and others.

## The Elixir AST

Elixir syntax was designed to have a straightforward conversion to an abstract syntax tree (AST). Elixir's AST is a regular Elixir data structure composed of the following elements:

  * atoms - such as `:foo`
  * integers - such as `42`
  * floats - such as `13.1`
  * strings - such as `"hello"`
  * lists - such as `[1, 2, 3]`
  * tuples with two elements - such as `{"hello", :world}`
  * tuples with three elements, representing calls or variables, as explained next

The building block of Elixir's AST is a call, such as:

```elixir
sum(1, 2, 3)
```

which is represented as a tuple with three elements:

```elixir
{:sum, meta, [1, 2, 3]}
```

the first element is an atom (or another tuple), the second element is a list of two-element tuples with metadata (such as line numbers) and the third is a list of arguments.

We can retrieve the AST for any Elixir expression by calling `quote`:

```elixir
quote do
  sum()
end
#=> {:sum, [], []}
```

Variables are also represented using a tuple with three elements and a combination of lists and atoms, for example:

```elixir
quote do
  sum
end
#=> {:sum, [], Elixir}
```

You can see that variables are also represented with a tuple, except the third element is an atom expressing the variable context.

Over the course of this section, we will explore many Elixir syntax constructs alongside their AST representations.

### Operators

Operators are treated as non-qualified calls:

```elixir
quote do
  1 + 2
end
#=> {:+, [], [1, 2]}
```

Note that `.` is also an operator. Remote calls use the dot in the AST with two arguments, where the second argument is always an atom:

```elixir
quote do
  foo.bar(1, 2, 3)
end
#=> {{:., [], [{:foo, [], Elixir}, :bar]}, [], [1, 2, 3]}
```

Calling anonymous functions uses the dot in the AST with a single argument, mirroring the fact the function name is "missing" from right side of the dot:

```elixir
quote do
  foo.(1, 2, 3)
end
#=> {{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}
```

### Aliases

Aliases are represented by an `__aliases__` call with each segment separated by a dot as an argument:

```elixir
quote do
  Foo.Bar.Baz
end
#=> {:__aliases__, [], [:Foo, :Bar, :Baz]}

quote do
  __MODULE__.Bar.Baz
end
#=> {:__aliases__, [], [{:__MODULE__, [], Elixir}, :Bar, :Baz]}
```

All arguments, except the first, are guaranteed to be atoms.

### Data structures

Remember that lists are literals, so they are represented as themselves in the AST:

```elixir
quote do
  [1, 2, 3]
end
#=> [1, 2, 3]
```

Tuples have their own representation, except for two-element tuples, which are represented as themselves:

```elixir
quote do
  {1, 2}
end
#=> {1, 2}

quote do
  {1, 2, 3}
end
#=> {:{}, [], [1, 2, 3]}
```

Binaries have a representation similar to tuples, except they are tagged with `:<<>>` instead of `:{}`:

```elixir
quote do
  <<1, 2, 3>>
end
#=> {:<<>>, [], [1, 2, 3]}
```

The same applies to maps, where pairs are treated as a list of tuples with two elements:

```elixir
quote do
  %{1 => 2, 3 => 4}
end
#=> {:%{}, [], [{1, 2}, {3, 4}]}
```

### Blocks

Blocks are represented as a `__block__` call with each line as a separate argument:

```elixir
quote do
  1
  2
  3
end
#=> {:__block__, [], [1, 2, 3]}

quote do 1; 2; 3; end
#=> {:__block__, [], [1, 2, 3]}
```

### Left to right arrow

The left to right arrow (`->`) is represented similar to operators except that they are always part of a list, its left side represents a list of arguments and the right side is an expression.

For example, in `case` and `cond`:

```elixir
quote do
  case 1 do
    2 -> 3
    4 -> 5
  end
end
#=> {:case, [], [1, [do: [{:->, [], [[2], 3]}, {:->, [], [[4], 5]}]]]}

quote do
  cond do
    true -> false
  end
end
#=> {:cond, [], [[do: [{:->, [], [[true], false]}]]]}
```

Between `(` and `)`:

```elixir
quote do
  (1, 2 -> 3
   4, 5 -> 6)
end
#=> [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]
```

Between `fn` and `end`:

```elixir
quote do
  fn
    1, 2 -> 3
    4, 5 -> 6
  end
end
#=> {:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}
```

### Qualified tuples

Qualified tuples (`foo.{bar, baz}`) are represented by a `{:., [], [expr, :{}]}` call, where the `expr` represents the left hand side of the dot, and the arguments represent the elements inside the curly braces. This is used in Elixir to provide multi aliases:

```elixir
quote do
  Foo.{Bar, Baz}
end
#=> {{:., [], [{:__aliases__, [], [:Foo]}, :{}]}, [], [{:__aliases__, [], [:Bar]}, {:__aliases__, [], [:Baz]}]}
```

### `do`-`end` blocks

Elixir's `do`-`end` blocks are equivalent to keywords as the last argument of a function call, where the block contents are wrapped in parentheses. For example:

```elixir
if true do
  this
else
  that
end
```

is the same as:

```elixir
if(true, do: (this), else: (that))
```

While the construct above does not require custom nodes in Elixir's AST, they are restricted only to certain keywords, listed next:

  * `after`
  * `catch`
  * `else`
  * `rescue`

You will find them in constructs such as `receive`, `try`, and others. You can also find more examples in [the Optional Syntax chapter](../getting-started/optional-syntax.md).
