<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Modules and functions

In Elixir we group several functions into modules. We've already used many different modules in the previous chapters, such as the `String` module:

```elixir
iex> String.length("hello")
5
```

In order to create our own modules in Elixir, we use the [`defmodule`](`defmodule/2`) macro. The first letter of an module name (an alias, as described further down) must be in uppercase. We use the [`def`](`def/2`) macro to define functions in that module. The first letter of every function must be in lowercase (or underscore):

```elixir
iex> defmodule Math do
...>   def sum(a, b) do
...>     a + b
...>   end
...> end

iex> Math.sum(1, 2)
3
```

In this chapter we will define our own modules, with different levels of complexity. As our examples get longer in size, it can be tricky to type them all in the shell, so we will resort more frequently to scripting.

## Scripting

Elixir has two file extensions `.ex` (Elixir) and `.exs` (Elixir scripts). Elixir treats both files exactly the same way, the only difference is in intention. `.ex` files are meant to be compiled while `.exs` files are used for scripting.

Let's create a file named `math.exs`:

```elixir
defmodule Math do
  def sum(a, b) do
    a + b
  end
end

IO.puts Math.sum(1, 2)
```

And execute it as:

```console
$ elixir math.exs
```

You can also load the file within `iex` by running:

```console
$ iex math.exs
```

And then have direct access to the `Math` module.

## Function definition

Inside a module, we can define functions with `def/2` and private functions with `defp/2`. A function defined with `def/2` can be invoked from other modules while a private function can only be invoked locally.

```elixir
defmodule Math do
  def sum(a, b) do
    do_sum(a, b)
  end

  defp do_sum(a, b) do
    a + b
  end
end

IO.puts Math.sum(1, 2)    #=> 3
IO.puts Math.do_sum(1, 2) #=> ** (UndefinedFunctionError)
```

Function declarations also support guards and multiple clauses. If a function has several clauses, Elixir will try each clause until it finds one that matches. Here is an implementation of a function that checks if the given number is zero or not:

```elixir
defmodule Math do
  def zero?(0) do
    true
  end

  def zero?(x) when is_integer(x) do
    false
  end
end

IO.puts Math.zero?(0)         #=> true
IO.puts Math.zero?(1)         #=> false
IO.puts Math.zero?([1, 2, 3]) #=> ** (FunctionClauseError)
IO.puts Math.zero?(0.0)       #=> ** (FunctionClauseError)
```

The trailing question mark in `zero?` means that this function returns a boolean. To learn more about the naming conventions for modules, function names, variables and more in Elixir, see [Naming Conventions](../references/naming-conventions.md).

Giving an argument that does not match any of the clauses raises an error.

Similar to constructs like `if`, function definitions support both `do:` and `do`-block syntax, as [we learned in the previous chapter](keywords-and-maps.md#do-blocks-and-keywords). For example, we can edit `math.exs` to look like this:

```elixir
defmodule Math do
  def zero?(0), do: true
  def zero?(x) when is_integer(x), do: false
end
```

And it will provide the same behavior. You may use `do:` for one-liners but always use `do`-blocks for functions spanning multiple lines. If you prefer to be consistent, you can use `do`-blocks throughout your codebase.

## Default arguments

Function definitions in Elixir also support default arguments:

```elixir
defmodule Concat do
  def join(a, b, sep \\ " ") do
    a <> sep <> b
  end
end

IO.puts(Concat.join("Hello", "world"))      #=> Hello world
IO.puts(Concat.join("Hello", "world", "_")) #=> Hello_world
```

Any expression is allowed to serve as a default value, but it won't be evaluated during the function definition. Every time the function is invoked and any of its default values have to be used, the expression for that default value will be evaluated:

```elixir
defmodule DefaultTest do
  def dowork(x \\ "hello") do
    x
  end
end
```

```elixir
iex> DefaultTest.dowork()
"hello"
iex> DefaultTest.dowork(123)
123
iex> DefaultTest.dowork()
"hello"
```

If a function with default values has multiple clauses, it is required to create a function head (a function definition without a body) for declaring defaults:

```elixir
defmodule Concat do
  # A function head declaring defaults
  def join(a, b, sep \\ " ")

  def join(a, b, _sep) when b == "" do
    a
  end

  def join(a, b, sep) do
    a <> sep <> b
  end
end

IO.puts(Concat.join("Hello", ""))           #=> Hello
IO.puts(Concat.join("Hello", "world"))      #=> Hello world
IO.puts(Concat.join("Hello", "world", "_")) #=> Hello_world
```

When a variable is not used by a function or a clause, we add a leading underscore (`_`) to its name to signal this intent. This rule is also covered in our [Naming Conventions](../references/naming-conventions.md#underscore-_foo) document.

## Understanding Aliases

An alias in Elixir is a capitalized identifier (like `String`, `Keyword`, etc) which is converted to an atom during compilation. For instance, the `String` alias translates by default to the atom `:"Elixir.String"`:

```elixir
iex> is_atom(String)
true
iex> to_string(String)
"Elixir.String"
iex> :"Elixir.String" == String
true
```

By using the `alias/2` directive, we are changing the atom the alias expands to.

Aliases expand to atoms because in the Erlang Virtual Machine (and consequently Elixir) modules are always represented by atoms. By namespacing
those atoms elixir modules avoid conflicting with existing erlang modules.

```elixir
iex> List.flatten([1, [2], 3])
[1, 2, 3]
iex> :"Elixir.List".flatten([1, [2], 3])
[1, 2, 3]
```

That's the mechanism we use to call Erlang modules:

```elixir
iex> :lists.flatten([1, [2], 3])
[1, 2, 3]
```

## Module nesting

Now that we have talked about aliases, we can talk about nesting and how it works in Elixir. Consider the following example:

```elixir
defmodule Foo do
  defmodule Bar do
  end
end
```

The example above will define two modules: `Foo` and `Foo.Bar`. The second can be accessed as `Bar` inside `Foo` as long as they are in the same lexical scope.

If, later, the `Bar` module is moved outside the `Foo` module definition, it must be referenced by its full name (`Foo.Bar`) or an alias must be set using the `alias` directive discussed above.

**Note**: in Elixir, you don't have to define the `Foo` module before being able to define the `Foo.Bar` module, as they are effectively independent. The above could also be written as:

```elixir
defmodule Foo.Bar do
end

defmodule Foo do
  alias Foo.Bar
  # Can still access it as `Bar`
end
```

Aliasing a nested module does not bring parent modules into scope. Consider the following example:

```elixir
defmodule Foo do
  defmodule Bar do
    defmodule Baz do
    end
  end
end

alias Foo.Bar.Baz
# The module `Foo.Bar.Baz` is now available as `Baz`
# However, the module `Foo.Bar` is *not* available as `Bar`
```

As we will see in later chapters, aliases also play a crucial role in macros, to guarantee they are hygienic.
