<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
-->

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

To define a struct, the `defstruct/1` construct is used:

```elixir
iex> defmodule User do
...>   defstruct name: "John", age: 27
...> end
```

The keyword list used with `defstruct` defines what fields the struct will have along with their default values. Structs take the name of the module they're defined in. In the example above, we defined a struct named `User`.

We can now create `User` structs by using a syntax similar to the one used to create maps:

```elixir
iex> %User{}
%User{age: 27, name: "John"}
iex> %User{name: "Jane"}
%User{age: 27, name: "Jane"}
```

Structs provide *compile-time* guarantees that only the fields defined through `defstruct` will be allowed to exist in a struct:

```elixir
iex> %User{oops: :field}
** (KeyError) key :oops not found expanding struct: User.__struct__/1
```

## Accessing and updating structs

Structs share the same syntax for accessing and updating fields as maps of fixed keys:

```elixir
iex> john = %User{}
%User{age: 27, name: "John"}
iex> john.name
"John"
iex> jane = %{john | name: "Jane"}
%User{age: 27, name: "Jane"}
iex> %{jane | oops: :field}
** (KeyError) key :oops not found in: %User{age: 27, name: "Jane"}
```

When using the update syntax (`|`), Elixir is aware that no new keys will be added to the struct, allowing the maps underneath to share their structure in memory. In the example above, both `john` and `jane` share the same key structure in memory.

Structs can also be used in pattern matching, both for matching on the value of specific keys as well as for ensuring that the matching value is a struct of the same type as the matched value.

```elixir
iex> %User{name: name} = john
%User{age: 27, name: "John"}
iex> name
"John"
iex> %User{} = %{}
** (MatchError) no match of right hand side value: %{}
```

For more details on creating, updating, and pattern matching structs, see the documentation for `%/2`.

## Dynamic struct updates

When you need to update structs with data from variables or external sources, use `struct!/2`:

```elixir
iex> john = %User{name: "John", age: 27}
%User{age: 27, name: "John"}
iex> struct!(john, name: "Jane", age: 30)
%User{age: 30, name: "Jane"}
```

Unlike the update syntax, `struct!/2` accepts data from maps and keyword lists, and will raise an error if you try to set invalid fields:

```elixir
iex> fields = [name: "Jane", invalid: "field"]
[name: "Jane", invalid: "field"]
iex> struct!(john, fields)
** (KeyError) key :invalid not found in: %User{age: 27, name: "John"}
```

Always use `struct!/2` instead of `Map` functions when working with structs, as functions like `Map.put/3` and `Map.merge/2` can break struct integrity. See the [`Kernel.struct!/2`](https://hexdocs.pm/elixir/Kernel.html#struct!/2) documentation for more details.

## Structs are bare maps underneath

Structs are simply maps with a "special" field named `__struct__` that holds the name of the struct:

```elixir
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

## Default values and required keys

If you don't specify a default key value when defining a struct, `nil` will be assumed:

```elixir
iex> defmodule Product do
...>   defstruct [:name]
...> end
iex> %Product{}
%Product{name: nil}
```

You can define a structure combining both fields with explicit default values, and implicit `nil` values. In this case you must first specify the fields which implicitly default to `nil`:

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
