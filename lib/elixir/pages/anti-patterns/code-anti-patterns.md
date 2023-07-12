# Code-related anti-patterns

This document outlines anti-patterns related to your code and particular Elixir idioms and features.

## Comments

#### Problem

When you overuse comments or comment self-explanatory code, it can have the effect of making code *less readable*.

#### Example

```elixir
# Returns the Unix timestamp of 5 minutes from the current time
defp unix_five_min_from_now do
  # Get the current time
  now = DateTime.utc_now()

  # Convert it to a Unix timestamp
  unix_now = DateTime.to_unix(now, :second)

  # Add five minutes in seconds
  unix_now + (60 * 5)
end
```

#### Refactoring

Prefer clear and self-explanatory function names, module names, and variable names when possible. In the example above, the function name explains well what the function does, so you likely won't need the comment before it. The code also explains the operations well through variable names and clear function calls.

You could refactor the code above like this:

```elixir
@five_min_in_seconds 60 * 5

defp unix_five_min_from_now do
  unix_now = DateTime.to_unix(DateTime.utc_now(), :second)
  unix_now + @five_min_in_seconds
end
```

We removed the unnecessary comments. We also added a `@five_min_in_seconds` module attribute, which serves the additional purpose of giving a name to the "magic" number `60 * 5`, making the code clearer and more expressive.

#### Additional remarks

Elixir makes a clear distinction between **documentation** and code comments. The language has built-in first-class support for documentation through `@doc`, `@moduledoc`, and more. See the ["Writing documentation"](../getting-started/writing-documentation.md) guide for more information.

## Long parameter list

TODO.

## Complex branching

#### Problem

When a function assumes the responsibility of handling multiple errors alone, it can increase its cyclomatic complexity (metric of control-flow) and become incomprehensible. This situation can configure a specific instance of "Long function", a traditional anti-pattern, but has implications of its own. Under these circumstances, this function could get very confusing, difficult to maintain and test, and therefore bug-proneness.

#### Example

An example of this anti-pattern is when a function uses the `case` control-flow structure or other similar constructs (for example, `cond`, or `receive`) to handle multiple variations of response types returned by the same API endpoint. This practice can make the function more complex, long, and difficult to understand, as shown next.

```elixir
def get_customer(customer_id) do
  case get("/customers/#{customer_id}") do
    {:ok, %Tesla.Env{status: 200, body: body}} -> {:ok, body}
    {:ok, %Tesla.Env{body: body}} -> {:error, body}
    {:error, _} = other -> other
  end
end
```

Although `get_customer/1` is not really long in this example, it could be. Thinking about this more complex scenario, where a large number of different responses can be provided to the same endpoint, is not a good idea to concentrate all on a single function. This is a risky scenario, where a little typo, or any problem introduced by the programmer in handling a response type, could eventually compromise the handling of all responses from the endpoint (if the function raises an exception, for example).

#### Refactoring

As shown below, in this situation, instead of concentrating all handlings within the same function, creating a complex branching, it is better to delegate each branch (handling of a response type) to a different private function. In this way, the code will be cleaner, more concise, and readable.

```elixir
def get_customer(customer_id) when is_integer(customer_id) do
  case get("/customers/#{customer_id}") do
    {:ok, %Tesla.Env{status: 200, body: body}} -> success_api_response(body)
    {:ok, %Tesla.Env{body: body}} -> x_error_api_response(body)
    {:error, _} = other -> y_error_api_response(other)
  end
end

defp success_api_response(body) do
  {:ok, body}
end

defp x_error_api_response(body) do
  {:error, body}
end

defp y_error_api_response(other) do
  other
end
```

While this example of refactoring `get_customer/1` might seem quite more verbose than the original code, remember to imagine a scenario where `get_customer/1` is responsible for handling a number much larger than three different types of possible responses. This is the smelly scenario!

## Complex `else` clauses in `with`

#### Problem

This anti-pattern refers to `with` statements that flatten all its error clauses into a single complex `else` block. This situation is harmful to the code readability and maintainability because difficult to know from which clause the error value came.

#### Example

An example of this anti-pattern, as shown below, is a function `open_decoded_file/1` that read a base 64 encoded string content from a file and returns a decoded binary string. This function uses a `with` statement that needs to handle two possible errors, all of which are concentrated in a single complex `else` block.

```elixir
def open_decoded_file(path) do
  with {:ok, encoded} <- File.read(path),
    {:ok, value} <- Base.decode64(encoded) do
    value
  else
    {:error, _} -> :badfile
    :error -> :badencoding
  end
end
```

#### Refactoring

In this situation, instead of concentrating all error handlings within a single complex `else` block, it is better to normalize the return types in specific private functions. In this way, due to its organization, the code will be cleaner and more readable.

```elixir
def open_decoded_file(path) do
  with {:ok, encoded} <- file_read(path),
    {:ok, value} <- base_decode64(encoded) do
    value
  end
end

defp file_read(path) do
  case File.read(path) do
    {:ok, contents} -> {:ok, contents}
    {:error, _} -> :badfile
  end
end

defp base_decode64(contents) do
  case Base.decode64(contents) do
    {:ok, contents} -> {:ok, contents}
    :error -> :badencoding
  end
end
```

## Complex extractions in clauses

TODO.

## Dynamic map fields access

TODO.

## Dynamic atom creation

TODO.

## Namespace trespassing

TODO.

## Speculative assumptions

TODO.
