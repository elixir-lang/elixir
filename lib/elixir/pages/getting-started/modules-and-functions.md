# Modules and functions

In Elixir we group several functions into modules. We've already used many different modules in the previous chapters, such as the `String` module:

```elixir
iex> String.length("hello")
5
```

In order to create our own modules in Elixir, we use the [`defmodule`](`defmodule/2`) macro. The first letter of the module must be in uppercase. We use the [`def`](`def/2`) macro to define functions in that module. The first letter of every function must be in lowercase (or underscore):

```elixir
iex> defmodule Math do
...>   def sum(a, b) do
...>     a + b
...>   end
...> end

iex> Math.sum(1, 2)
3
```

In this chapter we will define our own modules, with different levels of complexity. As our examples get longer in size, it can be tricky to type them all in the shell. It's about time for us to learn how to compile Elixir code and also how to run Elixir scripts.

## Compilation

Most of the time it is convenient to write modules into files so they can be compiled and reused. Let's assume we have a file named `math.ex` with the following contents:

```elixir
defmodule Math do
  def sum(a, b) do
    a + b
  end
end
```

This file can be compiled using `elixirc`:

```console
$ elixirc math.ex
```

This will generate a file named `Elixir.Math.beam` containing the bytecode for the defined module. If we start `iex` again, our module definition will be available (provided that `iex` is started in the same directory the bytecode file is in):

```elixir
iex> Math.sum(1, 2)
3
```

Elixir projects are usually organized into three directories:

  * `_build` - contains compilation artifacts
  * `lib` - contains Elixir code (usually `.ex` files)
  * `test` - contains tests (usually `.exs` files)

When working on actual projects, the build tool called `mix` will be responsible for compiling and setting up the proper paths for you. For learning and convenience purposes, Elixir also supports a scripting mode which is more flexible and does not generate any compiled artifacts.

## Scripting mode

In addition to the Elixir file extension `.ex`, Elixir also supports `.exs` files for scripting. Elixir treats both files exactly the same way, the only difference is in intention. `.ex` files are meant to be compiled while `.exs` files are used for scripting. This convention is followed by projects like `mix`.

For instance, we can create a file called `math.exs`:

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

Because we used `elixir` instead of `elixirc`, the module was compiled and loaded into memory, but no `.beam` file was written to disk. In the following examples, we recommend you write your code into script files and execute them as shown above.

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
  def join(a, b \\ nil, sep \\ " ")

  def join(a, b, _sep) when is_nil(b) do
    a
  end

  def join(a, b, sep) do
    a <> sep <> b
  end
end

IO.puts(Concat.join("Hello", "world"))      #=> Hello world
IO.puts(Concat.join("Hello", "world", "_")) #=> Hello_world
IO.puts(Concat.join("Hello"))               #=> Hello
```

When a variable is not used by a function or a clause, we add a leading underscore (`_`) to its name to signal this intent. This rule is also covered in our [Naming Conventions](../references/naming-conventions.md#underscore-_foo) document.

When using default values, one must be careful to avoid overlapping function definitions. Consider the following example:

```elixir
defmodule Concat do
  def join(a, b) do
    IO.puts("***First join")
    a <> b
  end

  def join(a, b, sep \\ " ") do
    IO.puts("***Second join")
    a <> sep <> b
  end
end
```

Elixir will emit the following warning:

```text
warning: this clause cannot match because a previous clause at line 2 always matches
    concat.ex:7: Concat
```

The compiler is telling us that invoking the `join` function with two arguments will always choose the first definition of `join` whereas the second one will only be invoked when three arguments are passed:

```console
$ iex concat.ex
```

```elixir
iex> Concat.join("Hello", "world")
***First join
"Helloworld"
```

```elixir
iex> Concat.join("Hello", "world", "_")
***Second join
"Hello_world"
```

Removing the default argument in this case will fix the warning.

This finishes our short introduction to modules. In the next chapters, we will learn how to use function definitions for recursion and later on explore more functionality related to modules.
