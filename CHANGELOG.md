# Changelog for Elixir v1.17

This release includes type inference of patterns to provide warnings for an initial set of constructs (binaries, maps, and atoms) within the same function. It also includes a new Duration data type to interact with Calendar types, support for Erlang/OTP 27, and many other improvements.

## Warnings from gradual set-theoretic types

This release introduces gradual set-theoretic types to infer types from patterns and use them to type check programs, enabling the Elixir compiler to find faults and bugs in codebases without requiring changes to existing software. The underlying principles, theory, and roadmap of our work have been outlined in ["The Design Principles of the Elixir Type System" by Giuseppe Castagna, Guillaume Duboc, José Valim](https://arxiv.org/abs/2306.06391).

At the moment, Elixir developers will interact with set-theoretic types only through warnings found by the type system. The current implementation models all data types in the language:

  * `binary()`, `integer()`, `float()`, `pid()`, `port()`, `reference()` - these types are indivisible. This means both `1` and `13` get the same `integer()` type.

  * `atom()` - it represents all atoms and it is divisible. For instance, the atom `:foo` and `:hello_world` are also valid (distinct) types.

  * `map()` and structs - maps can be "closed" or "open". Closed maps only allow the specified keys, such as `%{key: atom(), value: integer()}`. Open maps support any other keys in addition to the ones listed and their definition starts with `...`, such as `%{..., key: atom(), value: integer()}`. Structs are closed maps with the `__struct__` key.

  * `tuple()`, `list()`, and `function()` - currently they are modelled as indivisible types. The next Elixir versions will also introduce fine-grained support to them.

We focused on atoms and maps on this initial release as they are respectively the simplest and the most complex types representations, so we can stress the performance of the type system and quality of error messages. Modelling these types will also provide the most immediate benefits to Elixir developers. Assuming there is a variable named `user`, holding a `%User{}` struct with an `address` field, Elixir v1.17 will emit the following warnings at compile-time:

  * Pattern matching against a map or a struct that does not have the given key, such as `%{adress: ...} = user` (notice `address` vs `adress`)

  * Accessing a key on a map or a struct that does not have the given key, such as `user.adress`

  * Updating a struct or a map that does not define the given key, such as `%{user | adress: ...}`

  * Invoking a function on non-modules, such as `user.address()`

  * Capturing a function on non-modules, such as `&user.address/0`

  * Attempting to invoke to call an anonymous function without an actual function, such as `user.()`

  * Performing structural comparisons with structs, such as `my_date < ~D[2010-04-17]`

  * Performing structural comparisons between non-overlapping types, such as `integer >= string`

  * Building and pattern matching on binaries without the relevant specifiers, such as `<<name>>` (this warns because by default it expects an integer, it should have been `<<name::binary>>` instead)

  * Attempting to rescue an undefined exception or a struct that is not an exception

  * Accessing a field that is not defined in a rescued exception

These new warnings help Elixir developers find bugs earlier and give more confidence when refactoring code, especially around maps and structs. While some of these warnings were emitted in the past, they were discovered using syntax analysis. The new warnings are more reliable, precise, and with better error messages. Keep in mind, however, that the Elixir typechecker only infers types from patterns within the same function at the moment. Analysis from guards and across function boundaries will be added in future relases. For more details, see our new [reference document on gradual set-theoretic types](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html).

The type system was made possible thanks to a partnership between [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The development work is currently sponsored by [Fresha](https://www.fresha.com/), [Starfish*](https://starfish.team/), and [Dashbit](https://dashbit.co/).

## Erlang/OTP support

This release adds support for Erlang/OTP 27 and drops support for Erlang/OTP 24. We recommend Elixir developers to migrate to Erlang/OTP 26 or later, especially on Windows. Support for WERL (a graphical user interface for the Erlang terminal on Windows) will be removed in Elixir v1.18.

## Adding `Duration` and `shift/2` functions

Elixir introduces the `Duration` data type and APIs to shift dates, times, and date times by a given duration, considering different calendars and time zones.

```elixir
iex> Date.shift(~D[2016-01-31], month: 2)
~D[2016-03-31]
```

Note the operation is called `shift` (instead of `add`) since working with durations does not obey properties such as associativity. For instance, adding one month and then one month does not give the same result as adding two months:

```elixir
iex> ~D[2016-01-31] |> Date.shift(month: 1) |> Date.shift(month: 1)
~D[2016-03-29]
```

Still, durations are essential for building intervals, recurring events, and modelling scheduling complexities found in the world around us. For `DateTime`s, Elixir will correctly deal with time zone changes (such as Daylight Saving Time), but provisions are also available in case you want to surface conflicts (for example, you shifted to a wall clock that does not exist, because the clock has been moved forward by one hour). See `DateTime.shift/2` for examples.

Finally, a new `Kernel.to_timeout/1` function has been added, which helps developers normalize durations and integers to a timeout used by Process APIs. For example, to send a message after one hour, one can now write:

```elixir
Process.send_after(pid, :wake_up, to_timeout(hour: 1))
```

## v1.17.0 (2024-06-12)

### 1. Enhancements

#### Elixir

  * [Access] Add `Access.find/1` that mirrors `Enum.find/2`
  * [Code] Support cursor inside fn/rescue/catch/else/after inside `Code.Fragment.container_cursor_to_quoted/2`
  * [Date] Add `Date.shift/2` to shift dates with duration and calendar-specific semantics
  * [Date] Allow `Date` to accept years outside of `-9999..9999` range
  * [DateTime] Add `DateTime.shift/2` to shift datetimes with duration and calendar-specific semantics
  * [Duration] Add a new `Duration` data type
  * [GenServer] Add `c:GenServer.format_status/1` callback
  * [Kernel] Add `Kernel.get_in/1` with safe nil-handling for access and structs
  * [Kernel] Add `Kernel.is_non_struct_map/1` guard
  * [Kernel] Add `Kernel.to_timeout/1`
  * [Kernel] Emit warnings for undefined functions from modules defined within the same context as the caller code
  * [Kernel] Support integers in uppercase sigils
  * [Keyword] Add `Keyword.intersect/2-3` to mirror the `Map` API
  * [Macro] Add `Macro.Env.define_alias/4`, `Macro.Env.define_import/4`, `Macro.Env.define_require/4`, `Macro.Env.expand_alias/4`, `Macro.Env.expand_import/5`, and `Macro.Env.expand_require/6` to aid the implementation of language servers and embedded languages
  * [NaiveDateTime] Add `NaiveDateTime.shift/2` to shift naive datetimes with duration and calendar-specific semantics
  * [Process] Add `Process.set_label/1`
  * [String] Add `String.byte_slice/3` to slice a string to a maximum number of bytes while keeping it UTF-8 encoded
  * [System] Support `use_stdio: false` in `System.cmd/3` and `System.shell/2`
  * [Time] Add `Time.shift/2` to shift times with duration and calendar-specific semantics

#### ExUnit

  * [ExUnit] Propagate the test process itself as a caller in `start_supervised`
  * [ExUnit] Include max cases in ExUnit reports

#### IEx

  * [IEx.Helpers] Warns if `recompile` was called and the current working directory changed
  * [IEx.Helpers] Add `c/0` as an alias to `continue/0`
  * [IEx.Pry] Add `IEx.Pry.annotate_quoted/3` to annotate a quoted expression with pry breakpoints

#### Logger

  * [Logger] Format `:gen_statem` reports using Elixir data structures
  * [Logger] Include process label in logger events

#### Mix

  * [mix deps] Add `:depth` option to `Mix.SCM.Git`, thus supporting shallow clones of Git dependencies
  * [mix deps] Warn if `:optional` is used in combination with `:in_umbrella`
  * [mix deps.get] Do not add optional dependency requirements if its parent dep was skipped
  * [mix deps.tree] Add `--umbrella-only` to `mix deps.tree`
  * [mix profile.tprof] Add a new profiler, available on Erlang/OTP 27+, which can measure count, time, and heap usage
  * [mix test] Add `mix test --breakpoints` that sets up a breakpoint before each test that will run
  * [mix test] Add `mix test --repeat-until-failure` to rerun tests until a failure occurs
  * [mix test] Add `mix test --slowest-modules` to print slowest modules based on all of the tests they hold
  * [mix test] Generate cover HTML files in parallel


### 2. Bug fixes

#### Elixir

  * [bin/elixir.bat] Improve handling of quotes and exclamation marks in flags
  * [Code] Address a bug where AST nodes for `(a -> b)` were not wrapped as part of the literal encoder
  * [Kernel] Resolve inconsistencies of how `..` and `...` are handled at the AST level
  * [Kernel] Fix parsing precedence of ambiguous operators followed by containers
  * [Kernel] Do not expand code in `quote bind_quoted: ...` twice
  * [Kernel] Respect `:line` property when `:file` is given as option to `quote`
  * [Kernel] Do not crash on `Macro.escape/2` when passing a quote triplet without valid meta
  * [Kernel] Avoid double tracing events when capturing a function
  * [Kernel] Fix a bug where captured arguments would conflict when a capture included a macro that also used captures
  * [Module] Return default value in `Module.get_attribute/3` for persisted attributes which have not yet been written to

#### IEx

  * [IEx.Helpers] Update the history size whenever history is pruned

#### Mix

  * [mix deps] Fix error message for diverged SCM definition in sibling

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [GenServer] Deprecate `c:GenServer.format_status/2` callback to align with Erlang/OTP 25+

#### Mix

  * [mix profile.cprof] Deprecated in favor of the new `mix profile.tprof`
  * [mix profile.eprof] Deprecated in favor of the new `mix profile.tprof`

### 4. Hard deprecations

#### Elixir

  * [IO] Passing `:all` to `IO.read/2` and `IO.binread/2` is deprecated, pass `:eof` instead
  * [Kernel] Single-quote charlists are deprecated, use `~c` instead
  * [Kernel] Deprecate escaping closing delimiter in uppercase sigils
  * [Range] `left..right` without explicit steps inside patterns and guards is deprecated, write `left..right//step` instead
  * [Range] Decreasing ranges, such as `10..1` without an explicit step is deprecated, write `10..1//-1` instead

#### ExUnit

  * [ExUnit.Case] `register_test/4` is deprecated in favor of `register_test/6` for performance reasons

## v1.16

The CHANGELOG for v1.16 releases can be found [in the v1.16 branch](https://github.com/elixir-lang/elixir/blob/v1.16/CHANGELOG.md).
