# Code-related anti-patterns

This document outlines potential anti-patterns related to your code and particular Elixir idioms and features.

## Comments overuse

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
  now = DateTime.utc_now()
  unix_now = DateTime.to_unix(now, :second)
  unix_now + @five_min_in_seconds
end
```

We removed the unnecessary comments. We also added a `@five_min_in_seconds` module attribute, which serves the additional purpose of giving a name to the "magic" number `60 * 5`, making the code clearer and more expressive.

#### Additional remarks

Elixir makes a clear distinction between **documentation** and code comments. The language has built-in first-class support for documentation through `@doc`, `@moduledoc`, and more. See the ["Writing documentation"](../getting-started/writing-documentation.md) guide for more information.

## Complex `else` clauses in `with`

#### Problem

This anti-pattern refers to `with` expressions that flatten all its error clauses into a single complex `else` block. This situation is harmful to the code readability and maintainability because it's difficult to know from which clause the error value came.

#### Example

An example of this anti-pattern, as shown below, is a function `open_decoded_file/1` that reads a Base64-encoded string content from a file and returns a decoded binary string. This function uses a `with` expression that needs to handle two possible errors, all of which are concentrated in a single complex `else` block.

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

In the code above, it is unclear how each pattern on the left side of `<-` relates to their error at the end. The more patterns in a `with`, the less clear the code gets, and the more likely it is that unrelated failures will overlap each other.

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

When we use multi-clause functions, it is possible to extract values in the clauses for further usage and for pattern matching/guard checking. This extraction itself does not represent an anti-pattern, but when you have *extractions made across several clauses and several arguments of the same function*, it becomes hard to know which extracted parts are used for pattern/guards and what is used only inside the function body. This anti-pattern is related to [Unrelated multi-clause function](design-anti-patterns.md#unrelated-multi-clause-function), but with implications of its own. It impairs the code readability in a different way.

#### Example

The multi-clause function `drive/1` is extracting fields of an `%User{}` struct for usage in the clause expression (`age`) and for usage in the function body (`name`):

```elixir
def drive(%User{name: name, age: age}) when age >= 18 do
  "#{name} can drive"
end

def drive(%User{name: name, age: age}) when age < 18 do
  "#{name} cannot drive"
end
```

While the example above is small and does not constitute an anti-pattern, it is an example of mixed extraction and pattern matching. A situation where `drive/1` was more complex, having many more clauses, arguments, and extractions, would make it hard to know at a glance which variables are used for pattern/guards and which ones are not.

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

## Dynamic atom creation

#### Problem

An `Atom` is an Elixir basic type whose value is its own name. Atoms are often useful to identify resources or express the state, or result, of an operation. Creating atoms dynamically is not an anti-pattern by itself. However, atoms are not garbage collected by the Erlang Virtual Machine, so values of this type live in memory during a software's entire execution lifetime. The Erlang VM limits the number of atoms that can exist in an application by default to *1_048_576*, which is more than enough to cover all atoms defined in a program, but attempts to serve as an early limit for applications which are "leaking atoms" through dynamic creation.

For these reason, creating atoms dynamically can be considered an anti-pattern when the developer has no control over how many atoms will be created during the software execution. This unpredictable scenario can expose the software to unexpected behavior caused by excessive memory usage, or even by reaching the maximum number of *atoms* possible.

#### Example

Picture yourself implementing code that converts string values into atoms. These strings could have been received from an external system, either as part of a request into our application, or as part of a response to your application. This dynamic and unpredictable scenario poses a security risk, as these uncontrolled conversions can potentially trigger out-of-memory errors.

```elixir
defmodule MyRequestHandler do
  def parse(%{"status" => status, "message" => message} = _payload) do
    %{status: String.to_atom(status), message: message}
  end
end
```

```elixir
iex> MyRequestHandler.parse(%{"status" => "ok", "message" => "all good"})
%{status: :ok, message: "all good"}
```

When we use the `String.to_atom/1` function to dynamically create an atom, it essentially gains potential access to create arbitrary atoms in our system, causing us to lose control over adhering to the limits established by the BEAM. This issue could be exploited by someone to create enough atoms to shut down a system.

#### Refactoring

To eliminate this anti-pattern, developers must either perform explicit conversions by mapping strings to atoms or replace the use of `String.to_atom/1` with `String.to_existing_atom/1`. An explicit conversion could be done as follows:

```elixir
defmodule MyRequestHandler do
  def parse(%{"status" => status, "message" => message} = _payload) do
    %{status: convert_status(status), message: message}
  end

  defp convert_status("ok"), do: :ok
  defp convert_status("error"), do: :error
  defp convert_status("redirect"), do: :redirect
end
```

```elixir
iex> MyRequestHandler.parse(%{"status" => "status_not_seen_anywhere", "message" => "all good"})
** (FunctionClauseError) no function clause matching in MyRequestHandler.convert_status/1
```

By explicitly listing all supported statuses, you guarantee only a limited number of conversions may happen. Passing an invalid status will lead to a function clause error.

An alternative is to use `String.to_existing_atom/1`, which will only convert a string to atom if the atom already exists in the system:

```elixir
defmodule MyRequestHandler do
  def parse(%{"status" => status, "message" => message} = _payload) do
    %{status: String.to_existing_atom(status), message: message}
  end
end
```

```elixir
iex> MyRequestHandler.parse(%{"status" => "status_not_seen_anywhere", "message" => "all good"})
** (ArgumentError) errors were found at the given arguments:

  * 1st argument: not an already existing atom
```

In such cases, passing an unknown status will raise as long as the status was not defined anywhere as an atom in the system. However, assuming `status` can be either `:ok`, `:error`, or `:redirect`, how can you guarantee those atoms exist? You must ensure those atoms exist somewhere **in the same module** where `String.to_existing_atom/1` is called. For example, if you had this code:

```elixir
defmodule MyRequestHandler do
  def parse(%{"status" => status, "message" => message} = _payload) do
    %{status: String.to_existing_atom(status), message: message}
  end

  def handle(%{status: status}) do
    case status do
      :ok -> ...
      :error -> ...
      :redirect -> ...
    end
  end
end
```

All valid statuses are defined as atoms within the same module, and that's enough. If you want to be explicit, you could also have a function that lists them:

```elixir
def valid_statuses do
  [:ok, :error, :redirect]
end
```

However, keep in mind using a module attribute or defining the atoms in the module body, outside of a function, are not sufficient, as the module body is only executed during compilation and it is not necessarily part of the compiled module loaded at runtime.

## Long parameter list

#### Problem

In a functional language like Elixir, functions tend to explicitly receive all inputs and return all relevant outputs, instead of relying on mutations or side-effects. As functions grow in complexity, the amount of arguments (parameters) they need to work with may grow, to a point where the function's interface becomes confusing and prone to errors during use.

#### Example

In the following example, the `loan/6` functions takes too many arguments, causing its interface to be confusing and potentially leading developers to introduce errors during calls to this function.

```elixir
defmodule Library do
  # Too many parameters that can be grouped!
  def loan(user_name, email, password, user_alias, book_title, book_ed) do
    ...
  end
end
```

#### Refactoring

To address this anti-pattern, related arguments can be grouped using key-value data structures, such as maps, structs, or even keyword lists in the case of optional arguments. This effectively reduces the number of arguments and the key-value data structures adds clarity to the caller.

For this particular example, the arguments to `loan/6` can be grouped into two different maps, thereby reducing its arity to `loan/2`:

```elixir
defmodule Library do
  def loan(%{name: name, email: email, password: password, alias: alias} = user, %{title: title, ed: ed} = book) do
    ...
  end
end
```

In some cases, the function with too many arguments may be a private function, which gives us more flexibility over how to separate the function arguments. One possible suggestion for such scenarios is to split the arguments in two maps (or tuples): one map keeps the data that may change, and the other keeps the data that won't change (read-only). This gives us a mechanical option to refactor the code.

Other times, a function may legitimately take half a dozen or more completely unrelated arguments. This may suggest the function is trying to do too much and would be better broken into multiple functions, each responsible for a smaller piece of the overall responsibility.

## Namespace trespassing

#### Problem

This anti-pattern manifests when a package author or a library defines modules outside of its "namespace". A library should use its name as a "prefix" for all of its modules. For example, a package named `:my_lib` should define all of its modules within the `MyLib` namespace, such as `MyLib.User`, `MyLib.SubModule`, `MyLib.Application`, and `MyLib` itself.

This is important because the Erlang VM can only load one instance of a module at a time. So if there are multiple libraries that define the same module, then they are incompatible with each other due to this limitation. By always using the library name as a prefix, it avoids module name clashes due to the unique prefix.

#### Example

This problem commonly manifests when writing an extension of another library. For example, imagine you are writing a package that adds authentication to [Plug](https://github.com/elixir-plug/plug) called `:plug_auth`. You must avoid defining modules within the `Plug` namespace:

```elixir
defmodule Plug.Auth do
  # ...
end
```

Even if `Plug` does not currently define a `Plug.Auth` module, it may add such a module in the future, which would ultimately conflict with `plug_auth`'s definition.

#### Refactoring

Given the package is named `:plug_auth`, it must define modules inside the `PlugAuth` namespace:

```elixir
defmodule PlugAuth do
  # ...
end
```

#### Additional remarks

There are few known exceptions to this anti-pattern:

  * [Protocol implementations](`Kernel.defimpl/2`) are, by design, defined under the protocol namespace

  * In some scenarios, the namespace owner may allow exceptions to this rule. For example, in Elixir itself, you defined [custom Mix tasks](`Mix.Task`) by placing them under the `Mix.Tasks` namespace, such as `Mix.Tasks.PlugAuth`

  * If you are the maintainer for both `plug` and `plug_auth`, then you may allow `plug_auth` to define modules with the `Plug` namespace, such as `Plug.Auth`. However, you are responsible for avoiding or managing any conflicts that may arise in the future

## Non-assertive map access

#### Problem

In Elixir, it is possible to access values from `Map`s, which are key-value data structures, either statically or dynamically.

When a key is expected to exist in a map, it must be accessed using the `map.key` notation, making it clear to developers (and the compiler) that the key must exist. If the key does not exist, an exception is raised (and in some cases also compiler warnings). This is also known as the static notation, as the key is known at the time of writing the code.

When a key is optional, the `map[:key]` notation must be used instead. This way, if the informed key does not exist, `nil` is returned. This is the dynamic notation, as it also supports dynamic key access, such as `map[some_var]`.

When you use `map[:key]` to access a key that always exists in the map, you are making the code less clear for developers and for the compiler, as they now need to work with the assumption the key may not be there. This mismatch may also make it harder to track certain bugs. If the key is unexpectedly missing, you will have a `nil` value propagate through the system, instead of raising on map access.

#### Example

The function `plot/1` tries to draw a graphic to represent the position of a point in a Cartesian plane. This function receives a parameter of `Map` type with the point attributes, which can be a point of a 2D or 3D Cartesian coordinate system. This function uses dynamic access to retrieve values for the map keys:

```elixir
defmodule Graphics do
  def plot(point) do
    # Some other code...
    {point[:x], point[:y], point[:z]}
  end
end
```

```elixir
iex> point_2d = %{x: 2, y: 3}
%{x: 2, y: 3}
iex> point_3d = %{x: 5, y: 6, z: 7}
%{x: 5, y: 6, z: 7}
iex> Graphics.plot(point_2d)
{2, 3, nil}
iex> Graphics.plot(point_3d)
{5, 6, 7}
```

Given we want to plot both 2D and 3D points, the behavior above is expected. But what happens if we forget to pass a point with either `:x` or `:y`?

```elixir
iex> bad_point = %{y: 3, z: 4}
%{y: 3, z: 4}
iex> Graphics.plot(bad_point)
{nil, 3, 4}
```

The behavior above is unexpected because our function should not work with points without a `:x` key. This leads to subtle bugs, as we may now pass `nil` to another function, instead of raising early on.

#### Refactoring

To remove this anti-pattern, we must use the dynamic `map[:key]` syntax and the static `map.key` notation according to our requirements. We expect `:x` and `:y` to always exist, but not `:z`. The next code illustrates the refactoring of `plot/1`, removing this anti-pattern:

```elixir
defmodule Graphics do
  def plot(point) do
    # Some other code...
    {point.x, point.y, point[:z]}
  end
end
```

```elixir
iex> Graphics.plot(point_2d)
{2, 3, nil}
iex> Graphics.plot(bad_point)
** (KeyError) key :x not found in: %{y: 3, z: 4}
  graphic.ex:4: Graphics.plot/1
```

Overall, the usage of `map.key` and `map[:key]` encode important information about your data structure, allowing developers to be clear about their intent. See both `Map` and `Access` module documentation for more information and examples.

An alternative to refactor this anti-pattern is to use pattern matching, defining explicit clauses for 2d vs 3d points:

```elixir
defmodule Graphics do
  # 3d
  def plot(%{x: x, y: y, z: z}) do
    # Some other code...
    {x, y, z}
  end

  # 2d
  def plot(%{x: x, y: y}) do
    # Some other code...
    {x, y}
  end
end
```

Pattern-matching is specially useful when matching over multiple keys as well as on the values themselves at once.

Another option is to use structs. By default, structs only support static access to its fields. In such scenarios, you may consider defining structs for both 2D and 3D points:

```elixir
defmodule Point2D do
  @enforce_keys [:x, :y]
  defstruct [x: nil, y: nil]
end
```

Generally speaking, structs are useful when sharing data structures across modules, at the cost of adding a compile time dependency between these modules. If module `A` uses a struct defined in module `B`, `A` must be recompiled if the fields in the struct `B` change.

#### Additional remarks

This anti-pattern was formerly known as [Accessing non-existent map/struct fields](https://github.com/lucasvegi/Elixir-Code-Smells#accessing-non-existent-mapstruct-fields).

## Non-assertive pattern matching

#### Problem

Overall, Elixir systems are composed of many supervised processes, so the effects of an error are localized to a single process, and don't propagate to the entire application. A supervisor detects the failing process, reports it, and possibly restarts it. This anti-pattern arises when developers write defensive or imprecise code, capable of returning incorrect values which were not planned for, instead of programming in an assertive style through pattern matching and guards.

#### Example

The function `get_value/2` tries to extract a value from a specific key of a URL query string. As it is not implemented using pattern matching, `get_value/2` always returns a value, regardless of the format of the URL query string passed as a parameter in the call. Sometimes the returned value will be valid. However, if a URL query string with an unexpected format is used in the call, `get_value/2` will extract incorrect values from it:

```elixir
defmodule Extract do
  def get_value(string, desired_key) do
    parts = String.split(string, "&")

    Enum.find_value(parts, fn pair ->
      key_value = String.split(pair, "=")
      Enum.at(key_value, 0) == desired_key && Enum.at(key_value, 1)
    end)
  end
end
```

```elixir
# URL query string with the planned format - OK!
iex> Extract.get_value("name=Lucas&university=UFMG&lab=ASERG", "lab")
"ASERG"
iex> Extract.get_value("name=Lucas&university=UFMG&lab=ASERG", "university")
"UFMG"
# Unplanned URL query string format - Unplanned value extraction!
iex> Extract.get_value("name=Lucas&university=institution=UFMG&lab=ASERG", "university")
"institution"   # <= why not "institution=UFMG"? or only "UFMG"?
```

#### Refactoring

To remove this anti-pattern, `get_value/2` can be refactored through the use of pattern matching. So, if an unexpected URL query string format is used, the function will crash instead of returning an invalid value. This behavior, shown below, allows clients to decide how to handle these errors and doesn't give a false impression that the code is working correctly when unexpected values are extracted:

```elixir
defmodule Extract do
  def get_value(string, desired_key) do
    parts = String.split(string, "&")

    Enum.find_value(parts, fn pair ->
      [key, value] = String.split(pair, "=") # <= pattern matching
      key == desired_key && value
    end)
  end
end
```

```elixir
# URL query string with the planned format - OK!
iex> Extract.get_value("name=Lucas&university=UFMG&lab=ASERG", "name")
"Lucas"
# Unplanned URL query string format - Crash explaining the problem to the client!
iex> Extract.get_value("name=Lucas&university=institution=UFMG&lab=ASERG", "university")
** (MatchError) no match of right hand side value: ["university", "institution", "UFMG"]
  extract.ex:7: anonymous fn/2 in Extract.get_value/2 # <= left hand: [key, value] pair
iex> Extract.get_value("name=Lucas&university&lab=ASERG", "university")
** (MatchError) no match of right hand side value: ["university"]
  extract.ex:7: anonymous fn/2 in Extract.get_value/2 # <= left hand: [key, value] pair
```

Elixir and pattern matching promote an assertive style of programming where you handle the known cases. Once an unexpected scenario arises, you can decide to address it accordingly based on practical examples, or conclude the scenario is indeed invalid and the exception is the desired choice.

`case/2` is another important construct in Elixir that help us write assertive code, by matching on specific patterns. For example, if a function returns `{:ok, ...}` or `{:error, ...}`, prefer to explicitly match on both patterns:

```elixir
case some_function(arg) do
  {:ok, value} -> # ...
  {:error, _} -> # ...
end
```

In particular, avoid matching solely on `_`, as shown below:

```elixir
case some_function(arg) do
  {:ok, value} -> # ...
  _ -> # ...
end
```

 Matching on `_` is less clear in intent and it may hide bugs if `some_function/1` adds new return values in the future.

#### Additional remarks

This anti-pattern was formerly known as [Speculative assumptions](https://github.com/lucasvegi/Elixir-Code-Smells#speculative-assumptions).

## Non-assertive truthiness

#### Problem

Elixir provides the concept of truthiness: `nil` and `false` are considered "falsy" and all other values are "truthy". Many constructs in the language, such as `&&/2`, `||/2`, and `!/1` handle truthy and falsy values. Using those operators is not an anti-pattern. However, using those operators when all operands are expected to be booleans, may be an anti-pattern.

#### Example

The simplest scenario where this anti-pattern manifests is in conditionals, such as:

```elixir
if is_binary(name) && is_integer(age) do
  # ...
else
  # ...
end
```

Given both operands of `&&/2` are booleans, the code is more generic than necessary, and potentially unclear.

#### Refactoring

To remove this anti-pattern, we can replace `&&/2`, `||/2`, and `!/1` by `and/2`, `or/2`, and `not/1` respectively. These operators assert at least their first argument is a boolean:

```elixir
if is_binary(name) and is_integer(age) do
  # ...
else
  # ...
end
```

This technique may be particularly important when working with Erlang code. Erlang does not have the concept of truthiness. It never returns `nil`, instead its functions may return `:error` or `:undefined` in places an Elixir developer would return `nil`. Therefore, to avoid accidentally interpreting `:undefined` or `:error` as a truthy value, you may prefer to use `and/2`, `or/2`, and `not/1` exclusively when interfacing with Erlang APIs.

## Structs with 32 fields or more

#### Problem

Structs in Elixir are implemented as compile-time maps, which have a predefined amount of fields. When structs have 32 or more fields, their internal representation in the Erlang Virtual Machines changes, potentially leading to bloating and higher memory usage.

#### Example

Any struct with 32 or more fields will be problematic:

```elixir
defmodule MyExample do
  defstruct [
    :field1,
    :field2,
    ...,
    :field35
  ]
end
```

The Erlang VM has two internal representations for maps: a flat map and a hash map. A flat map is represented internally as two tuples: one tuple containing the keys and another tuple holding the values. Whenever you update a flat map, the tuple keys are shared, reducing the amount of memory used by the update. A hash map has a more complex structure, which is efficient for a large amount of keys, but it does not share the key space.

Maps of up to 32 keys are represented as flat maps. All others are hash map. Structs *are* maps (with a metadata field called `__struct__`) and so any struct with fewer than 32 fields is represented as a flat map. This allows us to optimize several struct operations, as we never add or remove fields to structs, we simply update them.

Furthermore, structs of the same name "instantiated" in the same module will share the same "tuple keys" at compilation times, as long as they have fewer than 32 fields. For example, the following code:

```elixir
defmodule Example do
  def users do
    [%User{name: "John"}, %User{name: "Meg"}, ...]
  end
end
```

All user structs will point to the same tuple keys at compile-time, also reducing the memory cost of instantiating structs with `%MyStruct{...}` notation. This optimization is also not available if the struct has 32 keys or more.

#### Refactoring

Removing this anti-pattern, in a nutshell, requires ensuring your struct has fewer than 32 fields. There are a few techniques you could apply:

* If the struct has "optional" fields, for example, fields which are initialized with nil, you could nest all optional fields into other field, called `:metadata`, `:optionals`, or similar. This could lead to benefits such as being able to use pattern matching to check if a field exists or not, instead of relying on `nil` values

* You could nest structs, by storing structs within other fields. Fields that are rarely read or written to are good candidates to be moved to a nested struct

* You could nest fields as tuples. For example, if two fields are always read or updated together, they could be moved to a tuple (or another composite data structure)

The challenge is to balance the changes above with API ergonomics, in particular, when fields may be frequentlyb read and written to.
