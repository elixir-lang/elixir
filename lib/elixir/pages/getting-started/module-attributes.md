# Module attributes

Module attributes in Elixir serve three purposes:

1. They serve to annotate the module, often with information to be used by the user or the VM.
2. They work as constants.
3. They work as a temporary module storage to be used during compilation.

Let's check each case, one by one.

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

You can take a look at the docs for `Module` for a complete list of supported attributes. Elixir also uses attributes to define [typespecs](../references/typespecs.md), which can be used to declare contracts between modules later.

## As "constants"

Elixir developers often use module attributes when they wish to make a value more visible or reusable:

```elixir
defmodule MyServer do
  @initial_state %{host: "127.0.0.1", port: 3456}
  IO.inspect @initial_state
end
```

Trying to access an attribute that was not defined will print a warning:

```elixir
defmodule MyServer do
  @unknown
end
warning: undefined module attribute @unknown, please remove access to @unknown or explicitly set it before access
```

Attributes can also be read inside functions:

```elixir
defmodule MyServer do
  @my_data 14
  def first_data, do: @my_data
  @my_data 13
  def second_data, do: @my_data
end

MyServer.first_data #=> 14
MyServer.second_data #=> 13
```

> Do not add a newline between the attribute and its value, otherwise Elixir will assume you are reading the value, rather than setting it.

Functions may be called when defining a module attribute:

```elixir
defmodule MyApp.Status do
  @service URI.parse("https://example.com")
  def status(email) do
    SomeHttpClient.get(@service)
  end
end
```

The function above will be called at compilation time and its *return value*, not the function call itself, is what will be substituted in for the attribute. So the above will effectively compile to this:

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

This can be useful for pre-computing constant values, but it can also cause problems if you're expecting the function to be called at runtime. For example, if you are reading a value from a database or an environment variable inside an attribute, be aware that it will read that value only at compilation time. However, note you cannot invoke functions defined in the same module as part of the attribute itself, as those functions have not yet been defined.

Every time an attribute is read inside a function, Elixir takes a snapshot of its current value. Therefore if you read the same attribute multiple times inside multiple functions, you may end-up making multiple copies of it. That's usually not an issue, but if you are using functions to compute large module attributes, that can slow down compilation. The solution is to move the attribute to shared function. For example, instead of this:

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

If `@example` is cheap to compute, it may be even better to skip the module attribute altogether, and compute its value inside the function.

### Accumulating attributes

Normally, repeating a module attribute will cause its value to be reassigned, but there are circumstances where you may want to [configure the module attribute](`Module.register_attribute/3`) so that its values are accumulated:

```elixir
defmodule Foo do
  Module.register_attribute(__MODULE__, :param, accumulate: true)

  @param :foo
  @param :bar
  # here @param == [:bar, :foo]
end
```

## As temporary storage

To see an example of using module attributes as storage, look no further than Elixir's unit test framework called `ExUnit`. ExUnit uses module attributes for multiple different purposes:

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

In the example above, `ExUnit` stores the value of `async: true` in a module attribute to change how the module is compiled. Tags are also defined as `accumulate: true` attributes, and they store tags that can be used to setup and filter tests. For example, you can avoid running external tests on your machine because they are slow and dependent on other services, while they can still be enabled in your build system.

In order to understand the underlying code, we'd need macros, so we will revisit this pattern in the meta-programming guide and learn how to use module attributes as storage to allow developers to create Domain Specific Languages (DSLs).

In the next chapters, we'll explore structs and protocols before moving to exception handling and other constructs like sigils and comprehensions.
