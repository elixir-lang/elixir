# Syntax reference

Here we document the syntax constructs in Elixir. We explore the base language constructs as well as the "syntax sugar" provided by Elixir and the underlying construct they desugar to.

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
sum(arg1, arg2, arg3)
```

which is represented as a tuple with three elements:

```elixir
{:sum, meta, args}
```

the first element is an atom (or another tuple), the second element is a list of two-item tuples with metadata (such as line numbers) and the third is a list of arguments.

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

Over the next section, we will explore many of Elixir syntax constructs alongside their AST representation.

### Numbers

Integers (`1234`) and floats (`123.4`) in Elixir are represented as a sequence of digits that may be separated by underscore for readability purposes, such as `1_000_000`. Integers never contain a dot (`.`) in their representation. Floats contain a dot and at least one other digit after the dot. Floats also support the scientific format, such as `123.4e10` or `123.4E10`.

Numbers are always represented as themselves in the AST:

```elixir
quote do
  1
end
#=> 1
```

### Atoms

Atoms in Elixir start with a colon (`:`) which must be followed by non-combining Unicode characters and underscore. The atom may continue using a sequence of Unicode characters, including numbers, underscore and `@`. Atoms may end in `!` or `?`. See [Unicode Syntax](unicode-syntax.html) for a formal specification.

All operators in Elixir are also valid atoms. Valid examples are `:foo`, `:FOO`, `:foo_42`, `:foo@bar` and `:++`. Invalid examples are `:@foo` (`@` is not allowed at start), `:123` (numbers are not allowed at start) and `:(*)` (not a valid operator).

If the colon is followed by a double- or single-quote, the atom can be made of any latin character up to OTP 19 or of any unicode character from OTP 20 onwards, such as `:"++olá++"`.

Atoms are always represented as themselves in the AST:

```elixir
quote do
  :foo
end
#=> :foo
```

### Strings

Strings in Elixir are written between double-quotes, such as `"foo"`. Any double-quote inside the string must be escaped with `\`. Strings support Unicode characters and are stored in UTF-8 encoding.

Strings are always represented as themselves in the AST.

### Charlists

Charlists in Elixir are written in single-quotes, such as `'foo'`. Any single-quote inside the string must be escaped with `\`. Charlists are a list of integers, each integer representing a Unicode character.

Charlists are always represented as themselves in the AST.

### Variables

Variables in Elixir must start with underscore or a non-combining Unicode character that is not in uppercase or titlecase. The variable may continue using a sequence of Unicode characters, including numbers and underscore. Variables may end in `?` or `!`. See [Unicode Syntax](unicode-syntax.html) for a formal specification.

[Elixir's naming conventions](naming-conventions.html) proposes variables to be in `snake_case` format.

Variables are represented by three-element tuples:

```elixir
quote do
  sum
end
#=> {:sum, [], Elixir}
```

### Non-qualified calls

Non-qualified calls, such as `add(1, 2)`, must start with lowercase characters which may be followed by any ASCII letter, number or underscore. Calls may end in `?` or `!`. [Elixir's naming conventions](naming-conventions.html) proposes function names to be in `snake_case` format.

Non-qualified calls are represented by three-element tuples:

```elixir
quote do
  sum(1, 2, 3)
end
#=> {:sum, [], [1, 2, 3]}
```

### Operators

Operators are treated as non-qualified calls:

```elixir
quote do
  1 + 2
end
#=> {:+, [], [1, 2]}
```

Notice that `.` is also an operator. Remote calls use the dot in the AST with two arguments, where the second argument is always an atom:

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

Many other Elixir constructs, such as `=`, `when`, `&` and `@` are simply treated as operators. See [the Operators page](operators.html) for a full reference.

### Aliases

Aliases are constructs that expand to atoms at compile-time. The alias `String` expands to the atom `:"Elixir.String"`. Aliases must start with an uppercase character which may be followed by any ASCII letter, number, or underscore. [Elixir's naming conventions](naming-conventions.html) propose aliases to be in `CamelCase` format.

Aliases are represented by an `__aliases__` call with each segment separated by dot as an argument:

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

All arguments, except the first, will be atoms.

### Qualified calls (remote calls)

Qualified calls, such as `Math.add(1, 2)`, must start with lowercase characters which may be followed by any ASCII letter, number or underscore. Calls may end in `?` or `!`. [Elixir's naming conventions](naming-conventions.html) propose function names to be in `snake_case` format.

For qualified calls, Elixir also allows the function name to be written between double- or single-quotes, allowing calls such as `Math."++add++"(1, 2)`. Operators can be used as qualified calls without a need for quote, such as `Kernel.+(1, 2)`.

Qualified calls are represented as a tuple with three elements in the AST where the first element is the a tuple reprsenting the dot:

```elixir
quote do
  :foo.bar(1, 2)
end
#=> {{:., [], [:foo, :bar]}, [], [1, 2]}
```

### Data structures

Data structures such as lists, tuples, and binaries are marked respectively by the delimiters `[...]`, `{...}`, and `<<...>>`. Each element is separated by comma. A trailing comma is also allowed, such as in `[1, 2, 3,]`.

Maps use the `%{...}` notation and each key-value is given by pairs marked with `=>`, such as `%{"hello" => 1, 2 => "world"}`.

Both maps and keyword lists support a notation for when the keys are atoms. Keywords are written using the same rules as atoms, except the colon character `:` is moved to the end, such as `%{hello: "world"}` and `[foo: :bar]`. This notation is a syntax sugar that emits the same AST representation. It will be explained in later sections.

Lists are represented as themselves in the AST:

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

The same applies to maps except pairs are treated as a list of tuples with two elements:

```elixir
quote do
  %{1 => 2, 3 => 4}
end
#=> {:%{}, [], [{1, 2}, {3, 4}]}
```

### Blocks

Blocks are multiple Elixir expressions separated by newlines. They are expanded to a `__block__` call with each line as a separate argument:

```elixir
quote do
  1
  2
  3
end
#=> {:__block__, [], [1, 2, 3]}
```

Expressions in Elixir are separated by newlines or semi-colons:

```elixir
quote do 1; 2; 3; end
#=> {:__block__, [], [1, 2, 3]}
```

### Left to right arrow

The left to right arrow (`->`) is used to establish a relationship between left and right. The left side may have zero, one or more arguments, the right side is an expression. The `->` is always between one of the following terminators: `do`/`end`, `fn`/`end` or `(`/`)`.

It is seen on `case` and `cond` constructs between `do`/`end`:

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

Seen in typespecs between `(`/`)`:

```elixir
quote do
  (1, 2 -> 3
   4, 5 -> 6)
end
#=> [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]
```

It is also used between `fn/end` for building anonymous functions:

```elixir
quote do
  fn
    1, 2 -> 3
    4, 5 -> 6
  end
end
#=> {:fn, [], [{:->, [], [[1, 2], 3]}, {:->, [], [[4, 5], 6]}]}
```

## Syntactic sugar

All of the constructs above are part of Elixir's syntax and have their own representation as part of the Elixir AST. This section will discuss the remaining constructs that "desugar" to one of the constructs explored above. In other words, the constructs below can be represented in more than one way in your Elixir code and retain AST equivalence.

### `true`, `false`, and `nil`

`true`, `false`, and `nil` are reserved words that are represented by the atoms `:true`, `:false` and `:nil` respectively.

### Integers in other bases and Unicode codepoints

Elixir allows integers to contain `_` to separate digits and provides conveniences to represent integers in other bases:

```elixir
1_000_000
#=> 1000000

0xABCD
#=> 43981 (Hexadecimal base)

0o01234567
#=> 342391 (Octal base)

0b10101010
#=> 170 (Binary base)

?é
#=> 233 (Unicode codepoint)
```

Those constructs exist only at the syntax level. All of the representations above are represented as integers in the AST.

### Optional parentheses

Elixir provides optional parentheses for non-qualified and qualified calls.

```elixir
quote do
  sum 1, 2, 3
end
#=> {:sum, [], [1, 2, 3]}
```

The above is treated the same as `sum(1, 2, 3)` by the parser.

The same applies to qualified calls such as `Foo.bar(1, 2, 3)`, which is the same as `Foo.bar 1, 2, 3`. However, keep in mind parentheses are not optional for local calls with no arguments, such as `sum()`. Removing the parentheses for `sum` causes it to be represented as the variable `sum`, changing its semantics.

### Access

The access syntax in Elixir, such as `foo[:bar]`, is treated as a shortcut to the remote call `Access.get(foo, :bar)`:

```elixir
quote do
  foo[:bar]
end
#=> {{:., [], [Access, :get]}, [], [{:foo, [], Elixir}, :bar]}
```

### Sigils

Sigils start with `~` and are followed by a letter and one of the following pairs:

  * `(` and `)`
  * `{` and `}`
  * `[` and `]`
  * `<` and `>`
  * `"` and `"`
  * `'` and `'`
  * `|` and `|`
  * `/` and `/`

After closing the pair, zero or more ASCII letters can be given as a modifier. Sigils are expressed as calls prefixed with `sigil_` where the first argument is the sigil contents as a string and the second argument is a list of integers as modifiers:

```elixir
quote do
  ~r/foo/
end
#=> {:sigil_r, [], [{:<<>>, [], ["foo"]}, []]}

quote do
  ~m/foo/abc
end
#=> {:sigil_m, [], [{:<<>>, [], ["foo"]}, 'abc']}
```

If the sigil letter is in uppercase, no interpolation is allowed in the sigil, otherwise its contents may be dynamic. Compare the quotes below for more information:

```elixir
quote do
  ~r/f#{"o"}o/
end

quote do
  ~R/f#{"o"}o/
end
```

### Keywords

Keywords in Elixir are a list of tuples of two elements where the first element is an atom. Using the base constructs, they would be represented as:

```elixir
[{:foo, 1}, {:bar, 2}]
```

However Elixir introduces a syntax sugar where the keywords above may be written as follows:

```elixir
[foo: 1, bar: 2]
```

Atoms with foreign characters in their name, such as whitespace, must be wrapped in quotes. This rule applies to keywords as well:

```elixir
[{:"foo bar", 1}, {:"bar baz", 2}] == ["foo bar": 1, "bar baz": 2]
```

Remember that, because lists and two-element tuples are quoted literals, by definition keywords are also literals (in fact, the only reason tuples with two elements are quoted literals is to support keywords as literals).

### Keywords as last arguments

Elixir also supports a syntax where if the last argument of a call is a keyword then the square brackets can be skipped. This means that the following:

```elixir
if(condition, do: this, else: that)
```

is the same as

```elixir
if(condition, [do: this, else: that])
```

which in turn is the same as

```elixir
if(condition, [{:do, this}, {:else, that}])
```

### `do`/`end` blocks

The last syntax convenience are `do`/`end` blocks. `do`/`end` blocks are equivalent to keywords where the block contents are wrapped in parentheses. For example:

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

which we have explored in the previous section.

Parentheses are important to support multiple expressions. This:

```elixir
if true do
  this
  that
end
```

is the same as:

```elixir
if(true, do: (
  this
  that
))
```

Inside `do`/`end` blocks you may introduce other keywords, such as `else` used in the `if` above. The supported keywords between `do`/`end` are static and are:

  * `after`
  * `catch`
  * `else`
  * `rescue`

You can see them being used in constructs such as `receive`, `try`, and others.

## Conclusion

This document provides a quick reference to Elixir syntax, exploring the simplicity behind its AST and documenting the base constructs with their AST equivalents.

We have also discussed a handful of syntax conveniences provided by Elixir. Those conveniences are what allow us to write

```elixir
defmodule Math do
  def add(a, b) do
    a + b
  end
end
```

instead of

```elixir
defmodule(Math, [
  {:do, def(add(a, b), [{:do, a + b}])}
])
```

The mapping between code and data (the underlying AST) is what allows Elixir to implement `defmodule`, `def`, `if`, and friends in Elixir itself. Making the constructs available for building the language also accessible to developers who want to extend the language to new domains.
