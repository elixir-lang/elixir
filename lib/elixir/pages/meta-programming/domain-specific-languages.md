<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

# Domain-Specific Languages (DSLs)

[Domain-specific Languages (DSLs)](https://en.wikipedia.org/wiki/Domain-specific_language) are languages tailored to a specific application domain. You don't need macros in order to have a DSL: every data structure and every function you define in your module is part of your domain-specific language.

For example, imagine we want to implement a `Validator` module which provides a data validation domain-specific language. We could implement it using data structures, functions, or macros. Let's see what those different DSLs would look like:

```elixir
# 1. Data structures
import Validator
validate user, name: [length: 1..100], email: [matches: ~r/@/]

# 2. Functions
import Validator
user
|> validate_length(:name, 1..100)
|> validate_matches(:email, ~r/@/)

# 3. Macros + modules
defmodule MyValidator do
  use Validator
  validate_length :name, 1..100
  validate_matches :email, ~r/@/
end

MyValidator.validate(user)
```

Of all the approaches above, the first is definitely the most flexible. If our domain rules can be encoded with data structures, they are by far the easiest to compose and implement, as Elixir's standard library is filled with functions for manipulating different data types.

The second approach uses function calls which better suits more complex APIs (for example, if you need to pass many options) and reads nicely in Elixir thanks to the pipe operator.

The third approach uses macros, and is by far the most complex. It will take more lines of code to implement, it is hard and expensive to test (compared to testing simple functions), and it limits how the user may use the library since all validations need to be defined inside a module.

To drive the point home, imagine you want to validate a certain attribute only if a given condition is met. We could easily achieve it with the first solution, by manipulating the data structure accordingly, or with the second solution by using conditionals (if/else) before invoking the function. However, it is impossible to do so with the macros approach unless its DSL is augmented.

In other words:

```text
data > functions > macros
```

That said, there are still cases where using macros and modules to build domain-specific languages is useful. Since we have explored data structures and function definitions in the Getting Started guide, this chapter will explore how to use macros and module attributes to tackle more complex DSLs.

## Building our own test case

The goal in this chapter is to build a module named `TestCase` that allows us to write the following:

```elixir
defmodule MyTest do
  use TestCase

  test "arithmetic operations" do
    4 = 2 + 2
  end

  test "list operations" do
    [1, 2, 3] = [1, 2] ++ [3]
  end
end

MyTest.run()
```

In the example above, by using `TestCase`, we can write tests using the `test` macro, which defines a function named `run` to automatically run all tests for us. Our prototype will rely on the match operator (`=`) as a mechanism to do assertions.

## The `test` macro

Let's start by creating a module that defines and imports the `test` macro when used:

```elixir
defmodule TestCase do
  # Callback invoked by `use`.
  #
  # For now it returns a quoted expression that
  # imports the module itself into the user code.
  @doc false
  defmacro __using__(_opts) do
    quote do
      import TestCase
    end
  end

  @doc """
  Defines a test case with the given description.

  ## Examples

      test "arithmetic operations" do
        4 = 2 + 2
      end

  """
  defmacro test(description, do: block) do
    function_name = String.to_atom("test " <> description)
    quote do
      def unquote(function_name)(), do: unquote(block)
    end
  end
end
```

Assuming we defined `TestCase` in a file named `tests.exs`, we can open it up by running `iex tests.exs` and define our first tests:

```elixir
iex> defmodule MyTest do
...>   use TestCase
...>
...>   test "hello" do
...>     "hello" = "world"
...>   end
...> end
```

For now, we don't have a mechanism to run tests, but we know that a function named `test hello` was defined behind the scenes. When we invoke it, it should fail:

```elixir
iex> MyTest."test hello"()
** (MatchError) no match of right hand side value: "world"
```

## Storing information with attributes

In order to finish our `TestCase` implementation, we need to be able to access all defined test cases. One way of doing this is by retrieving the tests at runtime via `__MODULE__.__info__(:functions)`, which returns a list of all functions in a given module. However, considering that we may want to store more information about each test besides the test name, a more flexible approach is required.

When discussing module attributes in earlier chapters, we mentioned how they can be used as temporary storage. That's exactly the property we will apply in this section.

In the `__using__/1` implementation, we will initialize a module attribute named `@tests` to an empty list, then store the name of each defined test in this attribute so the tests can be invoked from the `run` function.

Here is the updated code for the `TestCase` module:

```elixir
defmodule TestCase do
  @doc false
  defmacro __using__(_opts) do
    quote do
      import TestCase

      # Initialize @tests to an empty list
      @tests []

      # Invoke TestCase.__before_compile__/1 before the module is compiled
      @before_compile TestCase
    end
  end

  @doc """
  Defines a test case with the given description.

  ## Examples

      test "arithmetic operations" do
        4 = 2 + 2
      end

  """
  defmacro test(description, do: block) do
    function_name = String.to_atom("test " <> description)
    quote do
      # Prepend the newly defined test to the list of tests
      @tests [unquote(function_name) | @tests]
      def unquote(function_name)(), do: unquote(block)
    end
  end

  # This will be invoked right before the target module is compiled
  # giving us the perfect opportunity to inject the `run/0` function
  @doc false
  defmacro __before_compile__(_env) do
    quote do
      def run do
        Enum.each(@tests, fn name ->
          IO.puts("Running #{name}")
          apply(__MODULE__, name, [])
        end)
      end
    end
  end
end
```

By starting a new IEx session, we can now define our tests and run them:

```elixir
iex> defmodule MyTest do
...>   use TestCase
...>
...>   test "hello" do
...>     "hello" = "world"
...>   end
...> end
iex> MyTest.run()
Running test hello
** (MatchError) no match of right hand side value: "world"
```

Although we have overlooked some details, this is the main idea behind creating domain-specific languages in Elixir via modules and macros. Macros enable us to return quoted expressions that are executed in the caller, which we can then use to transform code and store relevant information in the target module via module attributes. Finally, callbacks such as `@before_compile` allow us to inject code into the module when its definition is complete.

Besides `@before_compile`, there are other useful module attributes like `@on_definition` and `@after_compile`, which you can read more about in the docs for `Module`. You can also find useful information about macros and the compilation environment in the documentation for the `Macro` and `Macro.Env`.
