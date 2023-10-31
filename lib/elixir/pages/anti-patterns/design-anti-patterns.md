# Design-related anti-patterns

This document outlines potential anti-patterns related to your modules, functions, and the role they
play within a codebase.

## Alternative return types

#### Problem

This anti-pattern refers to functions that receive options (typically as a *keyword list* parameter) that drastically change their return type. Because options are optional and sometimes set dynamically, if they also change the return type, it may be hard to understand what the function actually returns.

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

## Boolean obsession

#### Problem

This anti-pattern happens when booleans are used instead of atoms to encode information. The usage of booleans themselves is not an anti-pattern, but whenever multiple booleans are used with overlapping states, replacing the booleans by atoms (or composite data types such as *tuples*) may lead to clearer code.

This is a special case of [*Primitive obsession*](#primitive-obsession), specific to boolean values.

#### Example

An example of this anti-pattern is a function that receives two or more options, such as `editor: true` and `admin: true`, to configure its behaviour in overlapping ways. In the code below, the `:editor` option has no effect if `:admin` is set, meaning that the `:admin` option has higher priority than `:editor`, and they are ultimately related.

```elixir
defmodule MyApp do
  def process(invoice, options \\ []) do
    cond do
      options[:admin] ->  # Is an admin
      options[:editor] -> # Is an editor
      true ->          # Is none
    end
  end
end
```

#### Refactoring

Instead of using multiple options, the code above could be refactored to receive a single option, called `:role`, that can be either `:admin`, `:editor`, or `:default`:

```elixir
defmodule MyApp do
  def process(invoice, options \\ []) do
    case Keyword.get(options, :role, :default) do
      :admin ->   # Is an admin
      :editor ->  # Is an editor
      :default -> # Is none
    end
  end
end
```

This anti-pattern may also happen in our own data structures. For example, we may define a `User` struct with two boolean fields, `:editor` and `:admin`, while a single field named `:role` may be preferred.

Finally, it is worth noting that using atoms may be preferred even when we have a single boolean argument/option. For example, consider an invoice which may be set as approved/unapproved. One option is to provide a function that expects a boolean:

```elixir
MyApp.update(invoice, approved: true)
```

However, using atoms may read better and make it simpler to add further states (such as pending) in the future:

```elixir
MyApp.update(invoice, status: :approved)
```

Remember booleans are internally represented as atoms. Therefore there is no performance penalty in one approach over the other.

## Exceptions for control-flow

#### Problem

This anti-pattern refers to code that uses `Exception`s for control flow. Exception handling itself does not represent an anti-pattern, but developers must prefer to use `case` and pattern matching to change the flow of their code, instead of `try/rescue`. In turn, library authors should provide developers with APIs to handle errors without relying on exception handling. When developers have no freedom to decide if an error is exceptional or not, this is considered an anti-pattern.

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

Library authors are encouraged to follow the same practices. In practice, the bang variant is implemented on top of the non-raising version of the code. For example, `File.read!/1` is implemented as:

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

A common practice followed by the community is to make the non-raising version return `{:ok, result}` or `{:error, Exception.t}`. For example, an HTTP client may return `{:ok, %HTTP.Response{}}` on success cases and `{:error, %HTTP.Error{}}` for failures, where `HTTP.Error` is [implemented as an exception](`Kernel.defexception/1`). This makes it convenient for anyone to raise an exception by simply calling `Kernel.raise/1`.

## Primitive obsession

#### Problem

This anti-pattern happens when Elixir basic types (for example, *integer*, *float*, and *string*) are excessively used to carry structured information, rather than creating specific composite data types (for example, *tuples*, *maps*, and *structs*) that can better represent a domain.

#### Example

An example of this anti-pattern is the use of a single *string* to represent an `Address`. An `Address` is a more complex structure than a simple basic (aka, primitive) value.

```elixir
defmodule MyApp do
  def extract_postal_code(address) when is_binary(address) do
    # Extract postal code with address...
  end

  def fill_in_country(address) when is_binary(address) do
    # Fill in missing country...
  end
end
```

While you may receive the `address` as a string from a database, web request, or a third-party, if you find yourself frequently manipulating or extracting information from the string, it is a good indicator you should convert the address into structured data:

Another example of this anti-pattern is using floating numbers to model money and currency, when [richer data structures should be preferred](https://hexdocs.pm/ex_money/).

#### Refactoring

Possible solutions to this anti-pattern is to use maps or structs to model our address. The example below creates an `Address` struct, better representing this domain through a composite type. Additionally, we introduce a `parse/1` function, that converts the string into an `Address`, which will simplify the logic of remainng functions. With this modification, we can extract each field of this composite type individually when needed.

```elixir
defmodule Address do
  defstruct [:street, :city, :state, :postal_code, :country]
end
```

```elixir
defmodule MyApp do
  def parse(address) when is_binary(address) do
    # Returns %Address{}
  end

  def extract_postal_code(%Address{} = address) do
    # Extract postal code with address...
  end

  def fill_in_country(%Address{} = address) do
    # Fill in missing country...
  end
end
```

## Propagating invalid data

#### Problem

This anti-pattern refers to a function that does not validate its parameters and propagates them to other functions, which can produce internal unexpected behavior. When an error is raised inside a function due to an invalid parameter value, it can be confusing for developers and make it harder to locate and fix the error.

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

## Unrelated multi-clause function

#### Problem

Using multi-clause functions in Elixir, to group functions of the same name, is a powerful Elixir feature. However, some developers may abuse this feature to group *unrelated* functionality, which configures an anti-pattern.

#### Example

A frequent example of this usage of multi-clause functions is when developers mix unrelated business logic into the same function definition. Such functions often have generic names or too broad specifications, making it difficult for other developers to understand and maintain them.

Some developers may use documentation mechanisms such as `@doc` annotations to compensate for poor code readability, however the documentation itself may end-up full of conditionals to describe how the function behaves for each different argument combination. This is a good indicator that the clauses are ultimately unrelated.

```elixir
@doc """
Updates a struct.

If given a "sharp" product (metal or glass with empty count),
it will...

If given a blunt product, it will...

If given an animal, it will...
"""
def update(%Product{count: nil, material: material})
    when material in ["metal", "glass"] do
  # ...
end

def update(%Product{count: count, material: material})
    when count > 0 and material not in ["metal", "glass"] do
  # ...
end

def update(%Animal{count: 1, skin: skin})
    when skin in ["fur", "hairy"] do
  # ...
end
```

#### Refactoring

As shown below, a possible solution to this anti-pattern is to break the business rules that are mixed up in a single unrelated multi-clause function in several different simple functions. More precise names make the scope of the function clear. Each function can have a specific `@doc`, describing its behavior and parameters received. While this refactoring sounds simple, it can have a lot of impact on the function's current users, so be careful!

```elixir
@doc """
Updates a "sharp" product.

It will...
"""
def update_sharp_product(%Product{count: nil, material: material})
    when material in ["metal", "glass"] do
  # ...
end

@doc """
Updates a "blunt" product.

It will...
"""
def update_blunt_product(%Product{count: count, material: material})
    when count > 0 and material not in ["metal", "glass"] do
  # ...
end

@doc """
Updates an animal.

It will...
"""
def update_animal(%Animal{count: 1, skin: skin})
    when skin in ["fur", "hairy"] do
  # ...
end
```

## Using application configuration for libraries

#### Problem

The [*application environment*](https://hexdocs.pm/elixir/Application.html#module-the-application-environment) can be used to parameterize global values that can be used in an Elixir system. This mechanism can be very useful and therefore is not considered an anti-pattern by itself. However, library authors should avoid using the application environment to configure their library. The reason is exactly that the application environment is a **global** state, so there can only be a single value for each key in the environment for an application. This makes it impossible for multiple applications depending on the same library to configure the same aspect of the library in different ways.

#### Example

The `DashSplitter` module represents a library that configures the behavior of its functions through the global application environment. These configurations are concentrated in the *config/config.exs* file, shown below:

```elixir
import Config

config :app_config,
  parts: 3

import_config "#{config_env()}.exs"
```

One of the functions implemented by the `DashSplitter` library is `split/1`. This function aims to separate a string received via a parameter into a certain number of parts. The character used as a separator in `split/1` is always `"-"` and the number of parts the string is split into is defined globally by the application environment. This value is retrieved by the `split/1` function by calling `Application.fetch_env!/2`, as shown next:

```elixir
defmodule DashSplitter do
  def split(string) when is_binary(string) do
    parts = Application.fetch_env!(:app_config, :parts) # <= retrieve parameterized value
    String.split(string, "-", parts: parts)             # <= parts: 3
  end
end
```

Due to this parameterized value used by the `DashSplitter` library, all applications dependent on it can only use the `split/1` function with identical behavior about the number of parts generated by string separation. Currently, this value is equal to 3, as we can see in the use examples shown below:

```elixir
iex> DashSplitter.split("Lucas-Francisco-Vegi")
["Lucas", "Francisco", "Vegi"]
iex> DashSplitter.split("Lucas-Francisco-da-Matta-Vegi")
["Lucas", "Francisco", "da-Matta-Vegi"]
```

#### Refactoring

To remove this anti-pattern and make the library more adaptable and flexible, this type of configuration must be performed via parameters in function calls. The code shown below performs the refactoring of the `split/1` function by accepting [keyword lists](`Keyword`) as a new optional parameter. With this new parameter, it is possible to modify the default behavior of the function at the time of its call, allowing multiple different ways of using `split/2` within the same application:

```elixir
defmodule DashSplitter do
  def split(string, opts \\ []) when is_binary(string) and is_list(opts) do
    parts = Keyword.get(opts, :parts, 2) # <= default config of parts == 2
    String.split(string, "-", parts: parts)
  end
end
```

```elixir
iex> DashSplitter.split("Lucas-Francisco-da-Matta-Vegi", [parts: 5])
["Lucas", "Francisco", "da", "Matta", "Vegi"]
iex> DashSplitter.split("Lucas-Francisco-da-Matta-Vegi") #<= default config is used!
["Lucas", "Francisco-da-Matta-Vegi"]
```
