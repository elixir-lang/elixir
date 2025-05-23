<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Design-related anti-patterns

This document outlines potential anti-patterns related to your modules, functions, and the role they play within a codebase.

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
      case Integer.parse(string) do
        {int, _rest} -> int
        :error -> :error
      end
    else
      Integer.parse(string)
    end
  end
end
```

```elixir
iex> AlternativeInteger.parse("13")
{13, ""}
iex> AlternativeInteger.parse("13", discard_rest: false)
{13, ""}
iex> AlternativeInteger.parse("13", discard_rest: true)
13
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

An example of this anti-pattern is a function that receives two or more options, such as `editor: true` and `admin: true`, to configure its behavior in overlapping ways. In the code below, the `:editor` option has no effect if `:admin` is set, meaning that the `:admin` option has higher priority than `:editor`, and they are ultimately related.

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

This is only possible because the `File` module provides APIs for reading files with tuples as results (`File.read/1`), as well as a version that raises an exception (`File.read!/1`). The bang (exclamation point) is effectively part of [Elixir's naming conventions](naming-conventions.md#trailing-bang-foo).

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

#### Additional remarks

This anti-pattern is of special importance to library authors and whenever writing functions that will be invoked by other developers and third-party code. Nevertheless, there are still scenarios where developers can afford to raise exceptions directly, for example:

  * invalid arguments: it is expected that functions will raise for invalid arguments, as those are structural error and not semantic errors. For example, `File.read(123)` will always raise, because `123` is never a valid filename

  * during tests, scripts, etc: those are common scenarios where you want your code to fail as soon as possible in case of errors. Using `!` functions, such as `File.read!/1`, allows you to do so quickly and with clear error messages

  * some frameworks, such as [Phoenix](https://phoenixframework.org), allow developers to raise exceptions in their code and uses a protocol to convert these exceptions into semantic HTTP responses

This anti-pattern was formerly known as [Using exceptions for control-flow](https://github.com/lucasvegi/Elixir-Code-Smells#using-exceptions-for-control-flow).

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

Possible solutions to this anti-pattern is to use maps or structs to model our address. The example below creates an `Address` struct, better representing this domain through a composite type. Additionally, we introduce a `parse/1` function, that converts the string into an `Address`, which will simplify the logic of remaining functions. With this modification, we can extract each field of this composite type individually when needed.

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

## Unrelated multi-clause function

#### Problem

Using multi-clause functions is a powerful Elixir feature. However, some developers may abuse this feature to group *unrelated* functionality, which is an anti-pattern.

#### Example

A frequent example of this usage of multi-clause functions occurs when developers mix unrelated business logic into the same function definition, in a way that the behavior of each clause becomes completely distinct from the others. Such functions often have too broad specifications, making it difficult for other developers to understand and maintain them.

Some developers may use documentation mechanisms such as `@doc` annotations to compensate for poor code readability, however the documentation itself may end-up full of conditionals to describe how the function behaves for each different argument combination. This is a good indicator that the clauses are ultimately unrelated.

```elixir
@doc """
Updates a struct.

If given a product, it will...

If given an animal, it will...
"""
def update(%Product{count: count, material: material})  do
  # ...
end

def update(%Animal{count: count, skin: skin})  do
  # ...
end
```

If updating an animal is completely different from updating a product and requires a different set of rules, it may be worth splitting those over different functions or even different modules.

#### Refactoring

As shown below, a possible solution to this anti-pattern is to break the business rules that are mixed up in a single unrelated multi-clause function in simple functions. Each function can have a specific name and `@doc`, describing its behavior and parameters received. While this refactoring sounds simple, it can impact the function's callers, so be careful!

```elixir
@doc """
Updates a product.

It will...
"""
def update_product(%Product{count: count, material: material}) do
  # ...
end

@doc """
Updates an animal.

It will...
"""
def update_animal(%Animal{count: count, skin: skin}) do
  # ...
end
```

These functions may still be implemented with multiple clauses, as long as the clauses group related functionality. For example, `update_product` could be in practice implemented as follows:

```elixir
def update_product(%Product{count: 0}) do
  # ...
end

def update_product(%Product{material: material})
    when material in ["metal", "glass"] do
  # ...
end

def update_product(%Product{material: material})
    when material not in ["metal", "glass"] do
  # ...
end
```

You can see this pattern in practice within Elixir itself. The `+/2` operator can add `Integer`s and `Float`s together, but not `String`s, which instead use the `<>/2` operator. In this sense, it is reasonable to handle integers and floats in the same operation, but strings are unrelated enough to deserve their own function.

You will also find examples in Elixir of functions that work with any struct, which would seemingly be an occurrence of this anti-pattern, such as `struct/2`:

```elixir
iex> struct(URI.parse("/foo/bar"), path: "/bar/baz")
%URI{
  scheme: nil,
  userinfo: nil,
  host: nil,
  port: nil,
  path: "/bar/baz",
  query: nil,
  fragment: nil
}
```

The difference here is that the `struct/2` function behaves precisely the same for any struct given, therefore there is no question of how the function handles different inputs. If the behavior is clear and consistent for all inputs, then the anti-pattern does not take place.

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

To remove this anti-pattern, this type of configuration should be performed using a parameter passed to the function. The code shown below performs the refactoring of the `split/1` function by accepting [keyword lists](`Keyword`) as a new optional parameter. With this new parameter, it is possible to modify the default behavior of the function at the time of its call, allowing multiple different ways of using `split/2` within the same application:

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

Of course, not all uses of the application environment by libraries are incorrect. One example is using configuration to replace a component (or dependency) of a library by another that must behave the exact same. Consider a library that needs to parse CSV files. The library author may pick one package to use as default parser but allow its users to swap to different implementations via the application environment. At the end of the day, choosing a different CSV parser should not change the outcome, and library authors can even enforce this by [defining behaviours](../references/typespecs.md#behaviours) with the exact semantics they expect.

#### Additional remarks: Supervision trees

In practice, libraries may require additional configuration beyond keyword lists. For example, if a library needs to start a supervision tree, how can the user of said library customize its supervision tree? Given the supervision tree itself is global (as it belongs to the library), library authors may be tempted to use the application configuration once more.

One solution is for the library to provide its own child specification, instead of starting the supervision tree itself. This allows the user to start all necessary processes under its own supervision tree, potentially passing custom configuration options during initialization.

You can see this pattern in practice in projects like [Nx](https://github.com/elixir-nx/nx) and [DNS Cluster](https://github.com/phoenixframework/dns_cluster). These libraries require that you list processes under your own supervision tree:

```elixir
children = [
  {DNSCluster, query: "my.subdomain"}
]
```

In such cases, if the users of `DNSCluster` need to configure DNSCluster per environment, they can be the ones reading from the application environment, without the library forcing them to:

```elixir
children = [
  {DNSCluster, query: Application.get_env(:my_app, :dns_cluster_query) || :ignore}
]
```

Some libraries, such as [Ecto](https://github.com/elixir-ecto/ecto), allow you to pass your application name as an option (called `:otp_app` or similar) and then automatically read the environment from *your* application. While this addresses the issue with the application environment being global, as they read from each individual application, it comes at the cost of some indirection, compared to the example above where users explicitly read their application environment from their own code, whenever desired.

#### Additional remarks: Compile-time configuration

A similar discussion entails compile-time configuration. What if a library author requires some configuration to be provided at compilation time?

Once again, instead of forcing users of your library to provide compile-time configuration, you may want to allow users of your library to generate the code themselves. That's the approach taken by libraries such as [Ecto](https://github.com/elixir-ecto/ecto):

```elixir
defmodule MyApp.Repo do
  use Ecto.Repo, adapter: Ecto.Adapters.Postgres
end
```

Instead of forcing developers to share a single repository, Ecto allows its users to define as many repositories as they want. Given the `:adapter` configuration is required at compile-time, it is a required value on `use Ecto.Repo`. If developers want to configure the adapter per environment, then it is their choice:

```elixir
defmodule MyApp.Repo do
  use Ecto.Repo, adapter: Application.compile_env(:my_app, :repo_adapter)
end
```

On the other hand, [code generation comes with its own anti-patterns](macro-anti-patterns.md), and must be considered carefully. That's to say: while using the application environment for libraries is discouraged, especially compile-time configuration, in some cases they may be the best option. For example, consider a library needs to parse CSV or JSON files to generate code based on data files. In such cases, it is best to provide reasonable defaults and make them customizable via the application environment, instead of asking each user of your library to generate the exact same code.

#### Additional remarks: Mix tasks

For Mix tasks and related tools, it may be necessary to provide per-project configuration. For example, imagine you have a `:linter` project, which supports setting the output file and the verbosity level. You may choose to configure it through application environment:

```elixir
config :linter,
  output_file: "/path/to/output.json",
  verbosity: 3
```

However, `Mix` allows tasks to read per-project configuration via `Mix.Project.config/0`. In this case, you can configure the `:linter` directly in the `mix.exs` file:

```elixir
def project do
  [
    app: :my_app,
    version: "1.0.0",
    linter: [
      output_file: "/path/to/output.json",
      verbosity: 3
    ],
    ...
  ]
end
```

Additionally, if a Mix task is available, you can also accept these options as command line arguments (see `OptionParser`):

```bash
mix linter --output-file /path/to/output.json --verbosity 3
```
