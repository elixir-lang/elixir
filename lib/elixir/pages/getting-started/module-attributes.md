# Module attributes

Module attributes in Elixir serve three purposes:

1. as module and function annotations
2. as temporary module storage to be used during compilation
3. as compile-time constants

Let's check these examples.

## As annotations

Elixir brings the concept of module attributes from Erlang. For example:

```elixir
defmodule MyServer do
  @moduledoc "My server code."
end
```

In the example above, we are defining the module documentation by using the module attribute syntax. Elixir has a handful of reserved attributes. Here are a few of them, the most commonly used ones:

  * `@moduledoc` — provides documentation for the current module.
  * `@doc` — provides documentation for the function or macro that follows the attribute.
  * `@spec` — provides a typespec for the function that follows the attribute.
  * `@behaviour` — (notice the British spelling) used for specifying an OTP or user-defined behaviour.

`@moduledoc` and `@doc` are by far the most used attributes, and we expect you to use them a lot. Elixir treats documentation as first-class and provides many functions to access documentation. We will cover them [in their own chapter](writing-documentation.md).

Let's go back to the `Math` module defined in the previous chapters, add some documentation and save it to the `math.ex` file:

```elixir
defmodule Math do
  @moduledoc """
  Provides math-related functions.

  ## Examples

      iex> Math.sum(1, 2)
      3

  """

  @doc """
  Calculates the sum of two numbers.
  """
  def sum(a, b), do: a + b
end
```

Elixir promotes the use of Markdown with heredocs to write readable documentation. Heredocs are multi-line strings, they start and end with triple double-quotes, keeping the formatting of the inner text. We can access the documentation of any compiled module directly from IEx:

```console
$ elixirc math.ex
$ iex
```

```elixir
iex> h Math # Access the docs for the module Math
...
iex> h Math.sum # Access the docs for the sum function
...
```

We also provide a tool called [ExDoc](https://github.com/elixir-lang/ex_doc) which is used to generate HTML pages from the documentation.

You can take a look at the docs for `Module` for a complete list of supported attributes. Elixir also uses attributes to annotate our code with [typespecs](../references/typespecs.md).

## As temporary storage

So far, we have seen how to define attributes, but how can we read them? Let's see an example:

```elixir
defmodule MyServer do
  @service URI.parse("https://example.com")
  IO.inspect(@service)
end
```

> #### Newlines {: .warning}
>
> Do not add a newline between the attribute and its value, otherwise Elixir will assume you are reading the value, rather than setting it.

Trying to access an attribute that was not defined will print a warning:

```elixir
defmodule MyServer do
  @unknown
end
warning: undefined module attribute @unknown, please remove access to @unknown or explicitly set it before access
```

Attributes can also be read inside functions:

```elixir
defmodule MyApp.Status do
  @service URI.parse("https://example.com")
  def status(email) do
    SomeHttpClient.get(@service)
  end
end
```

The module attribute is defined at compilation time and its *return value*, not the function call itself, is what will be substituted in for the attribute. So the above will effectively compile to this:

```elixir
defmodule MyApp.Status do
  def status(email) do
    SomeHttpClient.get(%URI{
      authority: "example.com",
      host: "example.com",
      port: 443,
      scheme: "https"
    })
  end
end
```

This can be useful for pre-computing values and then injecting its results into the module. This is what we mean by temporary storage: after the module is compiled, the module attribute is discarded, except for the functions that have read the attribute. Note you cannot invoke functions defined in the same module as part of the attribute itself, as those functions have not yet been defined.

Every time we read an attribute inside a function, Elixir takes a snapshot of its current value. Therefore if you read the same attribute multiple times inside multiple functions, you end-up increasing compilation times as Elixir now has to compile every snapshot. Generally speaking, you want to avoid reading the same attribute multiple times and instead move it to function. For example, instead of this:

```elixir
def some_function, do: do_something_with(@example)
def another_function, do: do_something_else_with(@example)
```

Prefer this:

```elixir
def some_function, do: do_something_with(example())
def another_function, do: do_something_else_with(example())
defp example, do: @example
```

## As compile-time constants

Module attributes may also be useful as compile-time constants. Generally speaking, functions themselves are sufficient for the role of constants in a codebase. For example, instead of defining:

```elixir
@hours_in_a_day 24
```

You should prefer:

```elixir
defp hours_in_a_day(), do: 24
```

You may even define a public function if it needs to be shared across modules. It is common in many projects to have a module called `MyApp.Constants` that defines all constants used throughout the codebase.

You can even have composite data structures as constants, as long as they are made exclusively of other data types (no function calls, no operators, and no other expressions). For example, you may specify a system configuration constant as follows:

```elixir
defp system_config(), do: %{timezone: "Etc/UTC", locale: "pt-BR"}
```

Given data structures in Elixir are immutable, only a single instance of the data structure above is allocated and shared across all functions calls, as long as it doesn't have any executable expression.

The use case for module attributes arise when you need to do some work at compile-time and then inject its results inside a function. A common scenario is module attributes inside patterns and guards (as an alternative to `defguard/1`), since they only support a limited set of expressions:

```elixir
# Inside pattern
@default_timezone "Etc/UTC"
def shift(@default_timezone), do: ...

# Inside guards
@time_periods [:am, :pm]
def shift(time, period) when period in @time_periods, do: ...
```

Module attributes as constants and as temporary storage are most often used together: the module attribute is used to compute and store an expensive value, and then exposed as constant from that module.

## Going further

Libraries and frameworks can leverage module attributes to provide custom annotations. To see an example, look no further than Elixir's unit test framework called `ExUnit`. `ExUnit` uses module attributes for multiple different purposes:

```elixir
defmodule MyTest do
  use ExUnit.Case, async: true

  @tag :external
  @tag os: :unix
  test "contacts external service" do
    # ...
  end
end
```

In the example above, `ExUnit` stores the value of `async: true` in a module attribute to change how the module is compiled. Tags also work as annotations and they can be supplied multiple times, thanks to Elixir's ability to [accumulate attributes](`Module.register_attribute/3`). Then you can use tags to setup and filter tests, such as avoiding executing Unix specific tests while running your test suite on Windows.

To fully understand how `ExUnit` works, we'd need macros, so we will revisit this pattern in the Meta-programming guide and learn how to use module attributes as storage for custom annotations.

In the next chapters, we'll explore structs and protocols before moving to exception handling and other constructs like sigils and comprehensions.
