# Structs

We learned about maps [in earlier chapters](keywords-and-maps.md):

```elixir
iex> map = %{a: 1, b: 2}
%{a: 1, b: 2}
iex> map[:a]
1
iex> %{map | a: 3}
%{a: 3, b: 2}
```

Structs are extensions built on top of maps that provide compile-time checks and default values.

## Defining structs

To define a struct, the [`defstruct/1`](https://hexdocs.pm/elixir/Kernel.html#defstruct/1) construct is used:

```elixir
iex> defmodule User do
...>   defstruct name: "John", age: 27
...> end
```

The keyword list used with `defstruct` defines what fields the struct will have along with their default values. Structs take the name of the module they're defined in. In the example above, we defined a struct named `User`.

## Creating structs

We can create `User` structs by using a syntax similar to the one used to create maps:

```elixir
iex> %User{}
%User{age: 27, name: "John"}
iex> %User{name: "Jane"}
%User{age: 27, name: "Jane"}
```

Structs provide _compile-time_ guarantees that only the fields defined through `defstruct` will be allowed to exist in a struct:

```elixir
iex> %User{oops: :field}
** (KeyError) key :oops not found expanding struct: User.__struct__/1
```

You can also create structs using `Kernel.struct/2`:

```elixir
iex> struct(User)
%User{age: 27, name: "John"}
iex> struct(User, name: "Jane")
%User{age: 27, name: "Jane"}
iex> struct(User, %{name: "Jane", age: 30})
%User{age: 30, name: "Jane"}
```

### Default values and required keys

If you don't specify a default key value when defining a struct, `nil` will be assumed:

```elixir
iex> defmodule Product do
...>   defstruct [:name]
...> end
iex> %Product{}
%Product{name: nil}
```

You can define a structure combining both fields with explicit default values, and implicit `nil` values. In this case you must first specify the fields which implicitly default to nil:

```elixir
iex> defmodule User do
...>   defstruct [:email, name: "John", age: 27]
...> end
iex> %User{}
%User{age: 27, email: nil, name: "John"}
```

Doing it in reverse order will raise a syntax error:

```elixir
iex> defmodule User do
...>   defstruct [name: "John", age: 27, :email]
...> end
** (SyntaxError) iex:107: unexpected expression after keyword list. Keyword lists must always come last in lists and maps.
```

You can also enforce that certain keys have to be specified when creating the struct via the `@enforce_keys` module attribute:

```elixir
iex> defmodule Car do
...>   @enforce_keys [:make]
...>   defstruct [:model, :make]
...> end
iex> %Car{}
** (ArgumentError) the following keys must also be given when building struct Car: [:make]
    expanding struct: Car.__struct__/1
```

Enforcing keys provides a simple compile-time guarantee to aid developers when building structs. It is not enforced on updates and it does not provide any sort of value-validation.

## Accessing struct fields

Structs support dot notation for accessing fields:

```elixir
iex> john = %User{}
%User{age: 27, name: "John"}
iex> john.name
"John"
iex> john.age
27
```

Unlike maps, structs do not support bracket notation access:

```elixir
iex> john[:name]
** (UndefinedFunctionError) function User.fetch/2 is undefined (User does not implement the Access behaviour)
             User.fetch(%User{age: 27, name: "John"}, :name)
```

## Updating fields

When you need to update specific fields with known values, use the update syntax:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> jane = %{john | name: "Jane"}
%User{age: 27, name: "Jane"}
iex> older_jane = %{jane | age: 30, name: "Jane Smith"}
%User{age: 30, name: "Jane Smith"}
```

The update syntax ensures that only existing struct fields can be updated:

```elixir
iex> %{jane | oops: :field}
** (KeyError) key :oops not found in: %User{age: 27, name: "Jane"}
```

When using the update syntax (`|`), Elixir is aware that no new keys will be added to the struct, allowing the maps underneath to share their structure in memory. In the example above, both `john` and `jane` share the same key structure in memory.

## Merging data

When you have data in maps or keyword lists that you want to merge into a struct, use `Kernel.struct/2`. It accepts maps, keyword lists, or any enumerable that emits key-value pairs, and automatically discards keys that are not defined in the struct:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> fields = [name: "Jane", age: 30]
[name: "Jane", age: 30]
iex> struct(john, fields)
%User{age: 30, name: "Jane"}
iex> struct(john, %{name: "Jane", invalid_key: "ignored"})
%User{age: 27, name: "Jane"}
iex> struct(john, [name: "Jane", another_invalid: "also ignored"])
%User{age: 27, name: "Jane"}
```

## Pattern matching structs

Pattern matching structs is one of the most powerful features in Elixir. You can match on struct types, extract specific fields, and ensure data integrity all in one expression:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> %User{name: name} = john
%User{age: 27, name: "John"}
iex> name
"John"
iex> %User{} = %{}
** (MatchError) no match of right hand side value: %{}
```

Pattern matching structs is particularly useful in function heads to handle different data types:

```elixir
defmodule Notification do
  def send(%ChatMessage{user: user, text: text}) do
    "#{user}: #{text}"
  end

  def send(%FriendRequest{sender: sender}) do
    "#{sender} wants to be your friend"
  end

  def send(%GameInvite{sender: sender, game: game}) do
    "#{sender} invited you to play #{game}"
  end
end
```

For more details on creating, updating, and pattern matching structs, see the documentation for [`Kernel.SpecialForms.%/2`](https://hexdocs.pm/elixir/Kernel.SpecialForms.html#%25/2).

## Broken structs

Since structs are maps underneath, it's possible to change them using `Map` functions. However, this can "break" the struct by adding fields that were not defined in `defstruct`, turning it into a plain map:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> broken = Map.put(john, :email, "john@example.com")
%{__struct__: User, age: 27, email: "john@example.com", name: "John"}
iex> is_map(broken)
true
```

The result may look like a struct but it's actually a plain map with extra fields. This can lead to unexpected behavior when pattern matching or using protocols.

To safely merge data into structs, always use `Kernel.struct/2` instead of `Map` functions:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> struct(john, %{name: "Jane", email: "ignored"})
%User{age: 27, name: "Jane"}
```

## Structs vs maps

Structs are simply maps with a "special" field named `__struct__` that holds the name of the struct:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> is_map(john)
true
iex> john.__struct__
User
```

However, structs do not inherit any of the protocols that maps do. For example, you can neither enumerate nor access a struct:

```elixir
iex> john = %User{}
%User{age: 27, name: "John"}
iex> john[:name]
** (UndefinedFunctionError) function User.fetch/2 is undefined (User does not implement the Access behaviour)
             User.fetch(%User{age: 27, name: "John"}, :name)
iex> Enum.each(john, fn {field, value} -> IO.puts(value) end)
** (Protocol.UndefinedError) protocol Enumerable not implemented for %User{age: 27, name: "John"} of type User (a struct)
```

Structs alongside protocols provide one of the most important features for Elixir developers: data polymorphism. That's what we will explore in the next chapter.
