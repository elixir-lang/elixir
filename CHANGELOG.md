<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.20

## Type system improvements

This release includes type inference of all constructs.

### Type inference of function calls

Elixir now performs inference of whole functions. The best way to show the new capabilities are with examples. Take the following code:

```elixir
def add_foo_and_bar(data) do
  data.foo + data.bar
end
```

Elixir now infers that the function expects a `map` as first argument, and the map must have the keys `.foo` and `.bar` whose values are either `integer()` or `float()`. The return type will be either `integer()` or `float()`.

Here is another example:

```elixir
def sum_to_string(a, b) do
  Integer.to_string(a + b)
end
```

Even though the `+` operator works with both integers and floats, Elixir infers that `a` and `b` must be both integers, as the result of `+` is given to a function that expects an integer. The inferred type information is then used during type checking to find possible typing errors.

### Type inference of guards

This release also performs inference of guards! Let's see some examples:

```elixir
def example(x, y) when is_list(x) and is_integer(y)
```

The code above correctly infers `x` is a list and `y` is an integer.

```elixir
def example({:ok, x} = y) when is_binary(x) or is_integer(x)
```

The one above infers x is a binary or an integer, and `y` is a two element tuple with `:ok` as first element and a binary or integer as second.

```elixir
def example(x) when is_map_key(x, :foo)
```

The code above infers `x` is a map which has the `:foo` key, represented as `%{..., foo: dynamic()}`. Remember the leading `...` indicates the map may have other keys.

```elixir
def example(x) when not is_map_key(x, :foo)
```

And the code above infers `x` does not have the `:foo` key (hence `x.foo` will raise a typing violation), which has the type: `%{..., foo: not_set()}`.

You can also have expressions that assert on the size of data structures:

```elixir
def example(x) when tuple_size(x) < 3
```

Elixir will correctly track the tuple has at most two elements, and therefore accessing `elem(x, 3)` will emit a typing violation. In other words, Elixir can look at complex guards, infer types, and use this information to find bugs in our code, without a need to introduce type signatures (yet).

### Complete typing of maps keys

Maps were one of the first data-structures we implemented within the Elixir type system however, up to this point, they only supported atom keys. If they had additional keys, those keys were simply marked as `dynamic()`.

As of Elixir v1.20, we can track all possible domains as map keys. For example, the map:

```elixir
%{123 => "hello", 456.0 => :ok}
```

will have the type:

```elixir
%{integer() => binary(), float() => :ok}
```

It is also possible to mix domain keys, as above, with atom keys, yielding the following:

```elixir
%{integer() => integer(), root: integer()}
```

This system is an implementation of [Typing Records, Maps, and Structs, by Giuseppe Castagna (2023)](https://www.irif.fr/~gc/papers/icfp23.pdf).

### Typing of map operations

We have typed the majority of the functions in the `Map` module, allowing the type system to track how keys are added, updated, and removed across all possible key types.

For example, imagine we are calling the following `Map` functions with a variable `map`, which we don't know the exact shape of, and an atom key:

```elixir
Map.put(map, :key, 123)
#=> returns type %{..., key: integer()}

Map.delete(map, :key)
#=> returns type %{..., key: not_set()}
```

As you can see, we track when keys are set and also when they are removed.

Some operations, like `Map.replace/3`, only replace the key if it exists, and that is also propagated by the type system:

```elixir
Map.replace(map, :key, 123)
#=> returns type %{..., key: if_set(integer())}
```

In other words, if the key exists, it would have been replaced by an integer value. Furthermore, whenever calling a function in the `Map` module and the given key is statically proven to never exist in the map, an error is emitted.

By combining full type inference with bang operations like `Map.fetch!/2`, `Map.pop!/2`, `Map.replace!/3`, and `Map.update!/3`, Elixir is able to propagate information about the desired keys. Take this module:

```elixir
defmodule User do
  def name(map), do: Map.fetch!(map, :name)
end

defmodule CallsUser do
  def calls_name do
    User.name(%{})
  end
end
```

The code above has a type violation, which is now caught by the type system:

```text
    warning: incompatible types given to User.name/1:

        User.name(%{})

    given types:

        %{name: not_set()}

    but expected one of:

        dynamic(%{..., name: term()})

    type warning found at:
    │
 16 │     User.name(%{})
    │         ~
    │
    └─ lib/calls_user.ex:7:5: CallsUser.calls_name/0
```

### Acknowledgements

The type system was made possible thanks to a partnership between [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The development work is currently sponsored by [Fresha](https://www.fresha.com/) and [Tidewave](https://tidewave.ai/).

## v1.20.0-rc.1 (2026-01-13)

### 1. Bug fixes

#### Elixir

  * [Kernel] Improve the performance of the type system when working with large unions of open maps
  * [Kernel] Do not crash on map types with struct keys when performing type operations
  * [Kernel] Mark the outcome of bitstring types as dynamic
  * [Kernel] `<<expr::bitstring>>` will have type `binary` instead of `bitstring` if `expr` is a binary
  * [Kernel] Do not crash on conditional variables when calling a function on a module which is represented by a variable

## v1.20.0-rc.0 (2026-01-09)

### 1. Enhancements

#### Elixir

  * [Calendar] Optimize `date_from_iso_days` by using the Neri-Schneider algorithm
  * [Enum] Add `Enum.min_max` sorter
  * [Integer] Add `Integer.ceil_div/2`
  * [IO] Add `IO.iodata_empty?/1`
  * [File] Skip device, named pipes, etc in `File.cp_r/3` instead of erroring with reason `:eio`
  * [Kernel] Print intermediate results of `dbg` for pipes
  * [Kernel] Warn on unused requires
  * [Regex] Add `Regex.import/1` to import regexes defined with `/E`

#### ExUnit

  * [ExUnit.CaptureLog] Add `:formatter` option for custom log formatting

#### Mix

  * [mix deps] Support filtering `mix deps` output
  * [mix compile] Enforce `:elixirc_paths` to be a list of strings to avoid paths from being discarded (the only documented type was lists of strings)
  * [mix test] Add `mix test --dry-run`

### 2. Potential breaking changes

#### Elixir

  * `require SomeModule` no longer expands to the given module at compile-time, but it still returns the module at runtime. Note that while Elixir does not guarantee macros will expand to certain constructs, but since this can break code relying on the previous behaviour, such as `require(SomeMod).some_macro()`, we are adding this note to the CHANGELOG

### 3. Hard deprecations

#### Elixir

  * [File] `File.stream!(path, modes, lines_or_bytes)` is deprecated in favor of `File.stream!(path, lines_or_bytes, modes)`
  * [Kernel] Matching on the size inside a bit pattern now requires the pin operator for consistency, such as `<<x::size(^existing_var)>>`
  * [Kernel.ParallelCompiler] `Kernel.ParallelCompiler.async/1` is deprecated in favor of `Kernel.ParallelCompiler.pmap/2`, which is more performant and addresses known limitations

#### Logger

  * [Logger] `Logger.*_backend` functions are deprecated in favor of handlers. If you really want to keep on using backends, see the `:logger_backends` package
  * [Logger] `Logger.enable/1` and `Logger.disable/1` have been deprecated in favor of `Logger.put_process_level/2` and `Logger.delete_process_level/1`

## v1.19

The CHANGELOG for v1.19 releases can be found [in the v1.19 branch](https://github.com/elixir-lang/elixir/blob/v1.19/CHANGELOG.md).
