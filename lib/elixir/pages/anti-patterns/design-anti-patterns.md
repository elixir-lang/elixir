# Design-related anti-patterns

This document outlines anti-patterns related to your modules, functions, and the role they
play within a codebase.

## Primitive obsession

TODO

## Boolean obsession

TODO

## Working with invalid data

#### Problem

This anti-pattern refers to a function that does not validate its parameters' types and therefore can produce internal unexpected behavior. When an error is raised inside a function due to an invalid parameter value, it can be confusing for developers and make it harder to locate and fix the error.

#### Example

An example of this anti-pattern is when a function receives an invalid parameter and then passes it to other functions, either in the same library or in a third-party library. This can cause an error to be raised deep inside the call stack, which may be confusing for the developer who is working with invalid data. As shown next, the function `foo/1` is a user-facing API which doesn't validate its parameters at the boundary. In this way, it is possible that invalid data will be passed through, causing an error that is obscure and hard to debug.

```elixir
defmodule MyLibrary do
  def foo(invalid_data) do
    # Some other code...

    MyLibrary.Internal.sum(1, invalid_data)
  end
end
```

```elixir
iex> MyLibrary.foo(2)
3
iex> MyLibrary.foo("José") # With invalid data
** (ArithmeticError) bad argument in arithmetic expression: 1 + "José"
  :erlang.+(1, "José")
  my_library.ex:4: MyLibrary.Internal.sum/2
```

#### Refactoring

To remove this anti-pattern, the client code must validate input parameters at the boundary with the user, via guard clauses, pattern matching, or conditionals. This prevents errors from occurring elsewhere in the call stack, making them easier to understand and debug. This refactoring also allows libraries to be implemented without worrying about creating internal protection mechanisms. The next code snippet illustrates the refactoring of `foo/1`, removing this anti-pattern:

```elixir
defmodule MyLibrary do
  def foo(data) when is_integer(data) do
    # Some other code

    MyLibrary.Internal.sum(1, data)
  end
end
```

```elixir
iex> MyLibrary.foo(2) # With valid data
3
iex> MyLibrary.foo("José") # With invalid data
** (FunctionClauseError) no function clause matching in MyLibrary.foo/1.
The following arguments were given to MyLibrary.foo/1:

      # 1
      "José"

  my_library.ex:2: MyLibrary.foo/1
```

## Alternative return types

#### Problem

This anti-pattern refers to functions that receive options (for example, *keyword list*) parameters that drastically change their return type. Because options are optional and sometimes set dynamically, if they change the return type it may be hard to understand what the function actually returns.

#### Example

An example of this anti-pattern, as shown below, is when a function has many alternative return types, depending on the options received as a parameter.

```elixir
defmodule AlternativeInteger do
  @spec parse(String.t(), keyword()) :: integer() | {integer(), String.t()} | :error
  def parse(string, options \\ []) when is_list(options) do
    if Keyword.get(options, :discard_rest, false) do
      Integer.parse(string)
    else
      case Integer.parse(string) do
        {int, _rest} -> int
        :error -> :error
      end
    end
  end
end
```

```elixir
iex> AlternativeInteger.parse("13")
{13, ""}
iex> AlternativeInteger.parse("13", discard_rest: true)
13
iex> AlternativeInteger.parse("13", discard_rest: false)
{13, ""}
```

#### Refactoring

To refactor this anti-pattern, as shown next, add a specific function for each return type (for example, `parse_discard_rest/1`), no longer delegating this to options passed as arguments.

```elixir
defmodule AlternativeInteger do
  @spec parse(String.t()) :: {integer(), String.t()} | :error
  def parse(string) do
    Integer.parse(string)
  end
  
  @spec parse_discard_rest(String.t()) :: integer() | :error
  def parse_discard_rest(string) do
    case Integer.parse(string) do
      {int, _rest} -> int
      :error -> :error
    end
  end
end
```

```elixir
iex> AlternativeInteger.parse("13")
{13, ""}
iex> AlternativeInteger.parse_discard_rest("13")
13
```

## Unrelated multi-clause function

#### Problem

Using multi-clause functions in Elixir, to group functions of the same name, is not an anti-pattern in itself. However, due to the great flexibility provided by this programming feature, some developers may abuse the number of guard clauses and pattern matches to group *unrelated* functionality.

#### Example

A recurrent example of abusive use of multi-clause functions is when we’re trying to mix too much-unrelated business logic into the function definitions. This makes it difficult to read and understand the logic involved in the functions, which may impair code maintainability. Some developers use documentation mechanisms such as `@doc` annotations to compensate for poor code readability, but unfortunately, with a multi-clause function, we can only use these annotations once per function name, particularly on the first or header function. As shown next, all other variations of the function need to be documented only with comments, a mechanism that cannot automate tests, leaving the code prone to bugs.

```elixir
@doc """
Update sharp product with 0 or empty count

## Examples
iex> Namespace.Module.update(...)
expected result...
"""
def update(%Product{count: nil, material: material}) when material in ["metal", "glass"] do
  # ...
end

# update blunt product
def update(%Product{count: count, material: material}) when count > 0 and material in ["metal", "glass"] do
  # ...
end

# update animal...
def update(%Animal{count: 1, skin: skin}) when skin in ["fur", "hairy"] do
  # ...
end
```

#### Refactoring

As shown below, a possible solution to this anti-pattern is to break the business rules that are mixed up in a single unrelated multi-clause function in several different simple functions. Each function can have a specific `@doc`, describing its behavior and parameters received. While this refactoring sounds simple, it can have a lot of impact on the function's current users, so be careful!

```elixir
@doc """
Update sharp product

## Parameter
struct: %Product{...}

## Examples
iex> Namespace.Module.update_sharp_product(%Product{...})
expected result...
"""
def update_sharp_product(struct) do
  # ...
end

@doc """
Update blunt product

## Parameter
struct: %Product{...}

## Examples
iex> Namespace.Module.update_blunt_product(%Product{...})
expected result...
"""
def update_blunt_product(struct) do
  # ...
end

@doc """
Update animal

## Parameter
struct: %Animal{...}

## Examples
iex> Namespace.Module.update_animal(%Animal{...})
expected result...
"""
def update_animal(struct) do
  # ...
end
```

## Feature envy

TODO

## Excessive side-effects

TODO

## Using exceptions for control-flow

#### Problem

This anti-pattern refers to code that uses exceptions for control flow. Exception handling itself does not represent an anti-pattern, but developers must prefer to use `case` and pattern matching to change the flow of their code, instead of `try/rescue`. In turn, library authors should provide developers with APIs to handle errors without relying on exception handling. When developers have no freedom to decide if an error is exceptional or not, this is considered an anti-pattern.

#### Example

An example of this anti-pattern, as shown below, is using `try/rescue` to deal with file operations:

```elixir
defmodule MyModule do
  def print_file(file) do
    try do
      IO.puts(File.read!(file))
    rescue
      e -> IO.puts(:stderr, Exception.message(e))
    end
  end
end
```

```elixir
iex> MyModule.print_file("valid_file")
This is a valid file!
:ok
iex> MyModule.print_file("invalid_file")
could not read file "invalid_file": no such file or directory
:ok
```

#### Refactoring

To refactor this anti-pattern, as shown next, use `File.read/1`, which returns tuples instead of raising when a file cannot be read:

```elixir
defmodule MyModule do
  def print_file(file) do
    case File.read(file) do
      {:ok, binary} -> IO.puts(binary)
      {:error, reason} -> IO.puts(:stderr, "could not read file #{file}: #{reason}")
    end
  end
end
```

This is only possible because the `File` module provides APIs for reading files with tuples as results (`File.read/1`), as well as a version that raises an exception (`File.read!/1`). The bang (exclamation point) is effectively part of [Elixir's naming conventions](naming-conventions.html#trailing-bang-foo).

Library authors are encouraged to follow the same practices. In practice, the bang variant is implemented on top of the non-raising version of the code. For example, `File.read/1` is implemented as:

```elixir
def read!(path) do
  case read(path) do
    {:ok, binary} ->
      binary

    {:error, reason} ->
      raise File.Error, reason: reason, action: "read file", path: IO.chardata_to_string(path)
  end
end
```

A common practice followed by the community is to make the non-raising version to return `{:ok, result}` or `{:error, Exception.t}`. For example, an HTTP client may return `{:ok, %HTTP.Response{}}` on success cases and a `{:error, %HTTP.Error{}}` for failures, where `HTTP.Error` is [implemented as an exception](`Kernel.defexception/1`). This makes it convenient for anyone to raise an exception by simply calling `Kernel.raise/1`.

## Using application configuration for libraries

TODO
