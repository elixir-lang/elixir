<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.20

This release requires Erlang/OTP 27+ and is compatible with Erlang/OTP 29.

## Type system improvements

Elixir's type system now understands all language constructs and can infer types for your function definitions, using typing information from Elixir's standard library and your dependencies, to find verified bugs and dead code.

This has been achieved through a series of improvements, such as type refinement across clauses, occurrence typing, typing of map keys and domains, and more.

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

### Whole-body type inference

Elixir also performs inference based on the function body itself. Take the following code:

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

Even though the `+` operator works with both integers and floats, Elixir infers that `a` and `b` must be both integers, as the result of `+` is given to a function that expects an integer. The inferred type information is then used during type checking to find possible typing errors. The typing inferred from your dependencies are also used to help infer more precise types for your own applications.

### Typing across clauses

Elixir now infers the type of a given clause based on previous clauses. Let's see an example:

```elixir
case System.get_env("SOME_VAR") do
  nil -> :not_found
  value -> {:ok, String.upcase(value)}
end
```

`System.get_env("SOME_VAR")` returns either `nil` or a `binary()`. Because the first clause matches on `nil`, the type system knows `value` can no longer be `nil`, and therefore it must only be a `binary()`, which allows the second clause to also type check without violations.

This type inference across clauses also helps the type system find redundant clauses and dead code in existing codebases. Elixir v1.20 also implements occurrence typing for `cond`, `case`, and `with`, providing more precise types within each clause.

### Typing of atom and domain keys in maps

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

## Compile-time improvements

Elixir's v1.20 improves compilation times once more, especially on applications with many cores.

It also introduces a new compiler option called `:module_definition`, which if the module definition should be `:compiled` (the default) or `:interpreted`. Note this does not affect the `.beam` file written to disk, only how the contents inside `defmodule` are executed. Using the `:interpreted` mode may offer better compilation times for large projects, especially on machines with high core count, however, it comes with some downsides:

  * Errors during compilation may have less precise stacktraces

  * Anonymous functions within `defmodule` can have only up to 20 arguments.
    If this is an issue, you can use maps or tuples to group the data.
    Note the functions themselves inside `defmodule`, such as the ones defined
    inside `def` and friends, can still have up to 255 arguments

You can enable it by setting `elixirc_options: [module_definition: :interpreted]` in your `mix.exs`.

## v1.20.0-rc.6

This release requires Erlang/OTP 27+ and is compatible with Erlang/OTP 29.

### 1. Enhancements

#### Elixir

  * [Kernel] Perform type inference across applications

### 2. Bug fixes

#### Elixir

  * [Kernel] Fix type checker bug when validating a `case` inside a `cond` condition (regression)
  * [Kernel] Preserve evaluation order when rewriting function calls from Elixir modules into Erlang ones

#### Mix

  * [mix test] Respect --raise when mix test --warnings-as-errors passes with warnings

### 3. Hard deprecations

#### Mix

  * [mix compile.elixir] `xref: [exclude: ...]` in your `mix.exs` is deprecated in favor of `elixirc_options: [no_warn_undefined: ...]`

## v1.20.0-rc.5 (2026-05-13)

### 1. Enhancements

#### EEx

  * [EEx] Optimize compiler by flattening expr list only once

#### Elixir

  * [Base] Optimize Base validation functions by using SWAR techniques
  * [Float] Optimize `Float.round/2` by avoiding big integers
  * [Inspect] Increase inspect limit to help print deeply nested data structures
  * [Inspect] Support printing Erlang records (using Erlang notation)
  * [Kernel] Add occurrence typing on `case`, `cond`, and `with`
  * [Registry] Switch `{:duplicate, :key}` key_ets to ordered_set with composite keys
  * [String] SWAR-optimize ASCII fast paths in `String.length/1` and `String.slice/3`

#### ExUnit

  * [ExUnit] Show remaining runs when using `--repeat-until-failure`

#### IEx

  * [IEx.Helpers] Add `source/1`

#### Mix

  * [mix app.tree] Support `--output` option
  * [mix deps.tree] Support `--output` option
  * [mix help] Support printing docs for types and callbacks
  * [mix format] Support `--no-compile` option
  * [mix source] Add `mix source MODULE` to print or open a given module/function location

### 2. Potential breaking changes

#### Elixir

  * [Kernel] Disallow raw CR line ending in strings, comments and after `?` for security reasons

### 3. Bug fixes

#### Elixir

  * [Kernel] Fix a compiler crash when importing a module with `only: :sigils` option when the imported module exports non-sigil symbols with `sigil_` prefix
  * [Kernel] Reject negative Duration in `to_timeout/1`
  * [Macro] Fix generation of heredocs in `Macro.to_string/1` with escaped trailing newline
  * [Path] Consistently return path as binary in `Path.relative_to_cwd/2`
  * [Stream] Raise in `Stream.cycle/1` when enumerable reduce call yields no elements
  * [String] Support empty pattern list in `String.count/2`

#### Logger

  * [Logger] Persist log level to app env in `Logger.configure/1`

#### Mix

  * [Mix] Use `non_executable_binary_to_term` on loopback pubsub
  * [mix compile.elixir] Fix scenario where Elixir would tag mtimes in the future

## v1.20.0-rc.4 (2026-03-31)

This release requires Erlang/OTP 27+ and is compatible with Erlang/OTP 29.

### 1. Enhancements

#### Elixir

  * [Code] Add `:dbg_callback` option to eval functions
  * [Code.Fragment] Allow preserving sigil metadata in `container_cursor_to_quoted`
  * [File] Add support for `[:raw]` opts in `File.read/2`
  * [Kernel] Show undefined function errors even when missing variables (this helps debug errors caused when the developer forgets to require a macro)
  * [Module] Purge and delete modules if `after_compile/2` callback fails
  * [PartitionSupervisor] Support via tuples in `count_children/1` and `stop/3`
  * [Process] Add `Process.get_label/1`

#### Mix

  * [mix deps] Allow overriding specific dependencies in `:override`

### 2. Bug fixes

#### Elixir

  * [Integer] Fix `Integer.extended_gcd/2` returning negative GCD for zero base cases
  * [Integer] Raise when negative out-of-range digits are given to `Integer.undigits/2`
  * [Kernel] Protocols should not add compile-time dependencies on `Any` implementation
  * [Kernel] Ensure structs trigger recompilation for type checking purposes (regression)
  * [Kernel] Ensure type information propagate across `hd/tl` in guards (regression)
  * [Keyword] Raise `ArgumentError` in `Keyword.from_keys/2` for non-atom keys
  * [URI] Fix `URI.merge` leaking `:+` marker when base path is empty string

#### ExUnit

  * [ExUnit.Diff] Avoid false positives when diffing bitstrings

#### Mix

  * [mix deps] Use config files to pass project state to avoid argv limits on Windows when using `MIX_OS_DEPS_COMPILE_PARTITION_COUNT`
  * [mix compile] Fix compile env change triggering full recompilation of path dependencies
  * [mix compile] Add a build lock around protocol consolidation in umbrellas
  * [mix compile] Ensure compilation of sibling deps do not mark path deps as changed
  * [mix test] Fix `--warnings-as-errors` not catching misnamed test file warnings

## v1.20.0-rc.3 (2026-03-09)

### 1. Enhancements

#### IEx

  * [IEx] Optimize autocompleting modules

### 2. Bug fixes

#### Elixir

  * [Enum] Fix `Enum.slice/2` for ranges with step > 1 sliced by step > 1
  * [File] Allowing preserving directory permissions in `File.cp_r/3`
  * [File] Fix `File.cp_r/3` infinite loop with symlink cycles
  * [File] Fix `File.cp_r/3` infinite loop when copying into subdirectory of source
  * [File] Warn when defining `@type record()`, fixes CI on Erlang/OTP 29
  * [File] Fix `File.Stream` `Enumerable.count` for files without trailing newline
  * [Float] Fix `Float.parse/1` inconsistent error handling for non-scientific notation overflow
  * [Kernel] Process fields even when structs are unknown (regression)
  * [Kernel] Improve performance on several corner cases in the type system (regression)
  * [Kernel] Fix regression when using `Kernel.in/2` in defguard (regression)

## v1.20.0-rc.2 (2026-03-04)

### 1. Enhancements

#### Elixir

  * [Code] Add `module_definition: :interpreted` option to `Code` which allows module definitions to be evaluated instead of compiled. In some applications/architectures, this can lead to drastic improvements to compilation times. Note this does not affect the generated `.beam` file, which will have the same performance/behaviour as before
  * [Code] Make module purging opt-in and move temporary module deletion to the background to speed up compilation times
  * [Integer] Add `Integer.popcount/1`
  * [Kernel] Add type inference across clauses. For example, if one clause says `x when is_integer(x)`, then the next clause may no longer be an integer
  * [Kernel] Detect and warn on redundant clauses
  * [List] Add `List.first!/1` and `List.last!/1`
  * Add Software Bill of Materials guide to the Documentation

#### Mix

  * [mix compile] Add `module_definition: :interpreted` option to `Code` which allows module definitions to be evaluated instead of compiled. In some applications/architectures, this can lead to drastic improvements to compilation times. Note this does not affect the generated `.beam` file, which will have the same performance/behaviour as before
  * [mix deps] Parallelize dep lock status checks during `deps.loadpaths`, improving boot times in projects with many git dependencies

### 2. Bug fixes

#### IEx

  * [IEx] Ensure warnings emitted during IEx parsing are properly displayed/printed
  * [IEx] Ensure pry works across remote nodes

#### Mix

  * [mix compile.erlang] Topsort Erlang modules before compilation for proper dependency resolution

## v1.20.0-rc.1 (2026-01-13)

### 1. Bug fixes

#### Elixir

  * [Kernel] Do not crash on map types with struct keys when performing type operations (regression)
  * [Kernel] Mark the outcome of bitstring types as dynamic (regression)
  * [Kernel] `<<expr::bitstring>>` will have type `binary` instead of `bitstring` if `expr` is a binary (regression)
  * [Kernel] Do not crash on conditional variables when calling a function on a module which is represented by a variable (regression)

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

  * `require SomeModule` no longer expands to the given module at compile-time, but it still returns the module at runtime. Note Elixir does not guarantee macros will expand to certain constructs, but since this can break code relying on the previous behaviour, such as `require(SomeMod).some_macro()`, we are adding this note to the CHANGELOG

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
