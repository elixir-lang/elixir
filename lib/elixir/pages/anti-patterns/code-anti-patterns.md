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

#### Problem

When we use multi-clause functions, it is possible to extract values in the clauses for further usage and for pattern matching/guard checking. This extraction itself does not represent an anti-pattern, but when you have too many clauses or too many arguments, it becomes hard to know which extracted parts are used for pattern/guards and what is used only inside the function body. This anti-pattern is related to [Unrelated multi-clause function](design-anti-patterns.md#unrelated-multi-clause-function), but with implications of its own. It impairs the code readability in a different way.

#### Example

The multi-clause function `drive/1` is extracting fields of an `%User{}` struct for usage in the clause expression (`age`) and for usage in the function body (`name`). Ideally, a function should not mix pattern matching extractions for usage in its guard expressions and also in its body.

```elixir
def drive(%User{name: name, age: age}) when age >= 18 do
  "#{name} can drive"
end

def drive(%User{name: name, age: age}) when age < 18 do
  "#{name} cannot drive"
end
```

While the example is small and looks like a clear code, try to imagine a situation where `drive/1` was more complex, having many more clauses, arguments, and extractions.

#### Refactoring

As shown below, a possible solution to this anti-pattern is to extract only pattern/guard related variables in the signature once you have many arguments or multiple clauses:

```elixir
def drive(%User{age: age} = user) when age >= 18 do
  %User{name: name} = user
  "#{name} can drive"
end

def drive(%User{age: age} = user) when age < 18 do
  %User{name: name} = user
  "#{name} cannot drive"
end
```

## Dynamic map fields access

#### Problem

In Elixir, it is possible to access values from `Maps`, which are key-value data structures, either statically or dynamically. When trying to dynamically access the value of a key from a `Map`, if the informed key does not exist, `nil` is returned. This return can be confusing and does not allow developers to conclude whether the key is non-existent in the `Map` or just has no bound value. In this way, this anti-pattern may cause bugs in the code.

#### Example

The function `plot/1` tries to draw a graphic to represent the position of a point in a cartesian plane. This function receives a parameter of `Map` type with the point attributes, which can be a point of a 2D or 3D cartesian coordinate system. To decide if a point is 2D or 3D, this function uses dynamic access to retrieve values of the `Map` keys:

```elixir
defmodule Graphics do
  def plot(point) do
    #Some other code...

    # Dynamic access to use point values
    {point[:x], point[:y], point[:z]}
  end
end
```

```elixir
iex> point_2d = %{x: 2, y: 3}
%{x: 2, y: 3}
iex> point_3d = %{x: 5, y: 6, z: nil}
%{x: 5, y: 6, z: nil}
iex> Graphics.plot(point_2d)
{2, 3, nil}   # <= ambiguous return
iex> Graphics.plot(point_3d)
{5, 6, nil}
```

As can be seen in the example above, even when the key `:z` does not exist in the `Map` (`point_2d`), dynamic access returns the value `nil`. This return can be dangerous because of its ambiguity. It is not possible to conclude from it whether the `Map` has the key `:z` or not. If the function relies on the return value to make decisions about how to plot a point, this can be problematic and even cause errors when testing the code.

#### Refactoring

To remove this anti-pattern, whenever a `Map` has keys of `Atom` type, replace the dynamic access to its values by the `map.field`syntax. When a non-existent key is statically accessed, Elixir raises an error immediately, allowing developers to find bugs faster. The next code illustrates the refactoring of `plot/1`, removing this anti-pattern:

```elixir
defmodule Graphics do
  def plot(point) do
    #Some other code...

    # Strict access to use point values
    {point.x, point.y, point.z}
  end
end
```

```elixir
iex> point_2d = %{x: 2, y: 3}
%{x: 2, y: 3}
iex> point_3d = %{x: 5, y: 6, z: nil}
%{x: 5, y: 6, z: nil}
iex> Graphics.plot(point_2d)
** (KeyError) key :z not found in: %{x: 2, y: 3} # <= explicitly warns that
  graphic.ex:6: Graphics.plot/1                  # <= the :z key does not exist!
iex> Graphics.plot(point_3d)
{5, 6, nil}
```

As shown below, another alternative to refactor this anti-pattern is to use pattern matching:

```elixir
defmodule Graphics do
  def plot(%{x: x, y: y, z: z}) do
    #Some other code...

    # Strict access to use point values
    {x, y, z}
  end
end
```

```elixir
iex> point_2d = %{x: 2, y: 3}
%{x: 2, y: 3}
iex> point_3d = %{x: 5, y: 6, z: nil}
%{x: 5, y: 6, z: nil}
iex> Graphics.plot(point_2d)
** (FunctionClauseError)  no function clause matching in Graphics.plot/1
  graphic.ex:2: Graphics.plot/1                  # <= the :z key does not exist!
iex> Graphics.plot(point_3d)
{5, 6, nil}
```

Another alternative is to use structs. By default, structs only support static access to its fields, promoting cleaner patterns:

```elixir
defmodule Point.2D do
  @enforce_keys [:x, :y]
  defstruct [x: nil, y: nil]
end
```

```elixir
iex> point = %Point.2D{x: 2, y: 3}
%Point.2D{x: 2, y: 3}
iex> point.x   # <= strict access to use point values
2
iex> point.z   # <= trying to access a non-existent key
** (KeyError) key :z not found in: %Point{x: 2, y: 3}
iex> point[:x] # <= by default, struct does not support dynamic access
** (UndefinedFunctionError) ... (Point does not implement the Access behaviour)
```

#### Additional remarks

This anti-pattern was formerly known as [Accessing non-existent Map/Struct fields](https://github.com/lucasvegi/Elixir-Code-Smells#accessing-non-existent-mapstruct-fields).

## Dynamic atom creation

TODO.

## Namespace trespassing

TODO.

## Speculative assumptions

TODO.
