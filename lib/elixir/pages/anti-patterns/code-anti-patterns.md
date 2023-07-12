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

An example of this anti-pattern is when a function uses the `case` control-flow structure or other similar constructs (for example, `cond` or `receive`) to handle variations of a return type. This practice can make the function more complex, long, and difficult to understand, as shown next.

```elixir
def get_customer(customer_id) do
  case SomeHTTPClient.get("/customers/#{customer_id}") do
    {:ok, %{status: 200, body: body}} ->
      case Jason.decode(body) do
        {:ok, decoded} ->
          %{
            "first_name" => first_name,
            "last_name" => last_name,
            "company" => company
          } = decoded

          customer =
            %Customer{
              id: customer_id,
              name: "#{first_name} #{last_name}",
              company: company
            }

          {:ok, customer}

        {:error, _} ->
          {:error, "invalid reponse body"}
      end

    {:error, %{status: status, body: body}} ->
      case Jason.decode(body) do
        %{"error" => message} when is_binary(message) ->
          {:error, message}

        %{} ->
          {:error, "invalid reponse with status #{status}"}
      end
  end
end
```

The code above is complex because the `case` clauses are long and often have their own branching logic in them. With the clauses spread out, it is hard to understand what each clause does individually and it is hard to see all of the different scenarios your code pattern matches on.

#### Refactoring

As shown below, in this situation, instead of concentrating all error handling within the same function, creating complex branches, it is better to delegate each branch to a different private function. In this way, the code will be cleaner and more readable.

```elixir
def get_customer(customer_id) do
  case SomeHTTPClient.get("/customers/#{customer_id}") do
    {:ok, %{status: 200, body: body}} ->
      http_customer_to_struct(customer_id, body)

    {:error, %{status: status, body: body}} ->
      http_error(status, body)
  end
end
```

Both `http_customer_to_struct(customer_id, body)` and `http_error(status, body)` above contains the previous branches refactored into private functions.

It is worth noting that this refactoring is trivial to perform in Elixir because clauses cannot define variables or otherwise affect their parent scope. Therefore, extracting any clause or branch to a private function is a matter of gathering all variables used in that branch and passing them as arguments to the new function.

## Complex `else` clauses in `with`

#### Problem

This anti-pattern refers to `with` statements that flatten all its error clauses into a single complex `else` block. This situation is harmful to the code readability and maintainability because difficult to know from which clause the error value came.

#### Example

An example of this anti-pattern, as shown below, is a function `open_decoded_file/1` that reads a Base64-encoded string content from a file and returns a decoded binary string. This function uses a `with` statement that needs to handle two possible errors, all of which are concentrated in a single complex `else` block.

```elixir
def open_decoded_file(path) do
  with {:ok, encoded} <- File.read(path),
       {:ok, decoded} <- Base.decode64(encoded) do
    {:ok, String.trim(decoded)}
  else
    {:error, _} -> {:error, :badfile}
    :error -> {:error, :badencoding}
  end
end
```

In the code above, it is unclear how each pattern on the left side of `<-` relates to their error at the end. The more patterns in a `with`, the less clear the code gets, and the more likely unrelated failures will overlap each other.

#### Refactoring

In this situation, instead of concentrating all error handling within a single complex `else` block, it is better to normalize the return types in specific private functions. In this way, `with` can focus on the success case and the errors are normalized closer to where they happen, leading to better organized and maintainable code.

```elixir
def open_decoded_file(path) do
  with {:ok, encoded} <- file_read(path),
       {:ok, decoded} <- base_decode64(encoded) do
    {:ok, String.trim(decoded)}
  end
end

defp file_read(path) do
  case File.read(path) do
    {:ok, contents} -> {:ok, contents}
    {:error, _} -> {:error, :badfile}
  end
end

defp base_decode64(contents) do
  case Base.decode64(contents) do
    {:ok, decoded} -> {:ok, decoded}
    :error -> {:error, :badencoding}
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
