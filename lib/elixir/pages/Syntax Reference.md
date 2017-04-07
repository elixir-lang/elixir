# Syntax reference

Here we document the syntax constructs in Elixir. We explore the base language constructs as well as the "syntax sugar" provided by Elixir and the underlying construct they desugar to.

## The Elixir AST

Elixir syntax was designed to have a straight-forward conversion to an abstract syntax tree (AST). Therefore the best way to study Elixir syntax constructs is to also analyze how they are represented at the AST level.

The building block of Elixir's AST is a call, such as:

```elixir
sum(arg1, arg2, arg3)
```

which is represented as:

```elixir
{:sum, meta, args}
```

where the first element is an atom (or another tuple), the second element is a list of two-item tuples with metadata (such as line numbers) and the third is a list of arguments.

We can retrieve the AST for any Elixir expression by calling `quote`:

```elixir
quote do
  sum()
end
#=> {:sum, [], []}
```

Variables are also a basic construct:

```elixir
quote do
  sum
end
#=> {:sum, [], Elixir}
```

You can see that variables are also represented with a tuple, except the third element is an atom expressing the variable context.

Elixir also has quoted literals, which are values that, when quoted, return themselves. They are:

  * atoms - such as `:foo`
  * integers - such as `42`
  * floats - such as `13.1`
  * strings - such as `"hello"`
  * lists - such as `[1, 2, 3]`
  * tuples with two elements - such as `{"hello", :world}`

All syntax constructs in the Elixir language are expressed with the terms above.

## Base constructs

Below follow the constructs in the Elixir syntax and their AST representation. We will skip the quoted literals just introduced above.

### Variables

As introduced above:

```elixir
quote do
  sum
end
#=> {:sum, [], Elixir}
```

### Non-qualified calls

As introduced above:

```elixir
quote do
  sum(1, 2, 3)
end
#=> {:sum, [], [1, 2, 3]}
```

### Operators

Operators are expanded to calls.

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

Calling anonymous functions uses the dot in the AST with a single argument, mirroring the fact the second argument is "missing":

```elixir
quote do
  foo.(1, 2, 3)
end
#=> {{:., [], [{:foo, [], Elixir}]}, [], [1, 2, 3]}
```

Many other Elixir constructs, such as `=`, `when`, `&` and `@` are simply treated as operators. See [the Operators page](operators.html) for a full reference.

### Data-structures

Any data-structure in Elixir that is not a literal also has its own AST representation.

Tuples, except the ones with two elements, have their own representation:

```elixir
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

The same apply to maps except each pair is treated as a list of tuples with two elements:

```elixir
quote do
  %{1 => 2, 3 => 4}
end
#=> {:%{}, [], [{1, 2}, {3, 4}]}
```

### Blocks

Blocks are multiple Elixir expressions separated by new lines. They are expanded to a `__block__` call with each line as its own argument:

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

### Aliases

Aliases are compile-time constructs that expand to atoms. They are represented by an `__aliases__` call with each segment separated by dot as an argument:

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

After closing the pair, any ascii letter can be given as a modifier. Sigils are expressed as calls prefixed with `sigil_` where the first argument is the sigil contents as a string and the second argument is a list of integers as modifiers:

```elixir
quote do
  ~r/foo/
end
#=> {:sigil_r, [], [{:<<>>, [], ["foo"]}, []]}

quote do
  ~r/foo/abc
end
#=> {:sigil_r, [], [{:<<>>, [], ["foo"]}, 'abc']}
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

## Syntax sugar

All of the constructs above are part of Elixir's syntax and have their own representation as part of the Elixir AST. This section will discuss the remaining constructs that "desugar" to one of the constructs explored above. In other words, the constructs below can be represented in more than one way in your Elixir code and retain AST equivalence.

### true, false and nil

`true`, `false` and `nil` are reserved words that are represented by the atoms `:true`, `:false` and `:nil` respectively.

### Integers

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

All of those constructs are represented as integers in the AST.

### Optional parentheses

Elixir provides optional parantheses for non-qualified and qualified calls.

```elixir
quote do
  sum 1, 2, 3
end
#=> {:sum, [], [1, 2, 3]}
```

The above is treated the same as `sum(1, 2, 3)` by the parser.

The same applies to qualified calls where `Foo.bar(1, 2, 3)` is the same as `Foo.bar 1, 2, 3`. However, keep in mind parentheses are not optional for local calls with no arguments, such as `sum()`. Removing the parentheses for `sum` causes it to be represented as the variable `sum`, changing its semantics.

### Access

The access syntax in Elixir, such as `foo[:bar]` is treated as a shorcut to the remote call `Access.get(foo, :bar)`:

```elixir
quote do
  foo[:bar]
end
#=> {{:., [], [Access, :get]}, [], [{:foo, [], Elixir}, :bar]}
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

Atoms with foreign characters in their name, such as whitespace, must be wrapped in quotes. This same rule applies to keywords:

```elixir
[{:"foo bar", 1}, {:"bar baz", 2}] == ["foo bar": 1, "bar baz": 2]
```

Remember that, because lists and tuples of two elements are quoted literals, then by definition keywords are also literals (in fact, the only reason tuples with two elements are quoted literals is to support keywords as literals).

### Keywords as last arguments

Elixir also supports a syntax that, if the last argument of a call is a keyword, we can skip the square brackets. This means that:

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

### Blocks

The last syntax convenience are `do/end` blocks. `do/end` blocks is equivalent to keywords where the block contents are wrapped in parentheses. For example:

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

Notice the parentheses are important to support multiple expressions:

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

Notice `do`/`end` blocks we may introduce other keywords, such as `else` used in `if` above. The supported keywords between `do`/`end` are static and are made of: `after`, `catch`, `else` and `rescue`. You can see them being used in constructs such as `receive`, `try` and others.

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

The mapping between code and data (the underlying AST) is what allows Elixir to implement `defmodule`, `def`, `if` and friends in Elixir itself. Making the constructs available for building the language also accessible to developers who want to extend the language to new domains.