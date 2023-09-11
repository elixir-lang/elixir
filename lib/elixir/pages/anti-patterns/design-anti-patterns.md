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

TODO

## Feature envy

TODO

## Excessive side-effects

TODO

## Using exceptions for control-flow

#### Problem

This anti-pattern refers to code that forces developers to handle exceptions for control flow. Exception handling itself does not represent an anti-pattern, but this should not be the only alternative available to developers to handle an error in client code. When developers have no freedom to decide if an error is exceptional or not, this is considered an anti-pattern.

#### Example

An example of this anti-pattern, as shown below, is when a library forces its users to use `try/1` statements to rescue raised exceptions and handle different cases. Such a library library does not allow developers to decide if an error is exceptional or not in their applications.

```elixir
defmodule MyModule do
  def janky_function(value) do
    if is_integer(value) do
      "It's an integer"
    else
      raise "expected integer, got: #{inspect(value)}"
    end
  end
end
```

```elixir
defmodule Client do
  # Client forced to use exceptions for control flow.
  def print_janky_function(arg) do
    try do
      result = MyModule.janky_function(arg)
      "All good! #{result}."
    rescue
      exception in RuntimeError ->
        "Uh oh! #{exception.message}."
    end
  end
end
```

```elixir
iex> Client.print_janky_function(1)
"All good! It's an integer."
iex> Client.foo("Lucas")
"Uh oh! expected integer, got: \"Lucas\""
```

#### Refactoring

Library authors should guarantee that users are not required to use exceptions for control flow in their applications. As shown below, this can be done by refactoring `MyModule`, providing two versions of the function that forces clients to use exceptions for control flow: 

  1. A version that raises exceptions should have the same name as the "janky" one, but with a trailing `!` (`janky_function!/1`);
  
  2. Another version, without raised exceptions, should have a name identical to the original version (`janky_function/1`) and should return the result wrapped in a tuple.

```elixir
defmodule MyModule do
  def janky_function(value) do
    if is_integer(value) do
      {:ok, "It's an integer"}
    else
      {:error, "expected an integer, got: #{inspect(value)}"}
    end
  end

  def janky_function!(value) do
    case janky_function(value) do
      {:ok, result} -> result
      {:error, message} -> raise(message)
    end
  end
end
```

This refactoring gives users more freedom to decide how to proceed in the event of errors, defining what is exceptional or not in different situations. As shown next, when an error is not exceptional, clients can use specific control-flow structures, such as the `case/2` statement.

```elixir
defmodule Client do
  # Users now can also choose to use control-flow structures
  # for control flow when an error is not exceptional.
  def print_janky_function(arg) do
    case MyModule.janky_function(arg) do
      {:ok, value} -> "All good! #{value}."
      {:error, reason} -> "Uh oh! #{reason}."
    end
  end
end
```

```elixir
iex> Client.print_janky_function(1)
"All good! It's an integer."
iex> Client.print_janky_function("Lucas")
"Uh oh! expected an integer, got: \"Lucas\""
```

## Using application configuration for libraries

TODO
