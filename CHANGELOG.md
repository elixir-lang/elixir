# Changelog for Elixir v1.17

## Warnings from gradual set-theoretic types

This release introduces gradual set-theoretic types to infer types from patterns and guards and use them to type check programs, enabling the Elixir compiler to find faults and bugs in codebases without requiring changes to existing software. The underlying principles, theory, and roadmap of our work have been outlined in ["The Design Principles of the Elixir Type System" by Giuseppe Castagna, Guillaume Duboc, José Valim](https://arxiv.org/abs/2306.06391).

At the moment, Elixir developers will interact with set-theoretic types only through warnings found by the type system. The current implementation models all data types in the language:

  * `binary()`, `integer()`, `float()`, `pid()`, `port()`, `reference()` - these types are indivisible. This means both `1` and `13` get the same `integer()` type.

  * `atom()` - it represents all atoms and it is divisible. For instance, the atom `:foo` and `:hello_world` are also valid (distinct) types.

  * `map()` and structs - maps can be "closed" or "open". Closed maps only allow the specified allows keys, such as `%{key: atom(), value: integer()}`. Open maps support any other keys in addition to the ones listed and their definition starts with `...`, such as `%{..., key: atom(), value: integer()}`. Structs are closed maps with the `__struct__` key.

  * `tuple()`, `list()`, and `function()` - currently they are modelled as indivisible types. The next Elixir versions will also introduce fine-grained types here.

We focused on atoms and maps on this initial release as they are respectively the simplest and the most complex types representations, so we can stress the performance of the type system and quality of error messages. Modelling these types will also provide the most immediate benefits to Elixir developers. Assuming there is a variable named `user`, holding a `%User{}` struct with an `address` field, Elixir v1.17 will emit the following warnings at compile-time:

  * Pattern matching against a map or a struct that does not have the given key, such as `%{adress: ...} = user` (notice `address` vs `adress`)

  * Accessing a key on a map or a struct that does not have the given key, such as `user.adress`

  * Updating a struct or a map that does not define the given key, such as `%{user | adress: ...}`

  * Invoking a function on non-modules, such as `user.address()`

  * Capturing a function on non-modules, such as `&user.address/0`

  * Performing structural comparisons with structs, such as `my_date < ~D[2010-04-17]`

  * Performing structural comparisons between non-overlapping types, such as `integer >= string`

  * Building and pattern matching on binaries without the relevant specifiers, such as `<<string>>` (this warns because by default it expects an integer)

  * Attempting to rescue an undefined exception or an exception that is not a struct

  * Accessing a field that is not defined in a rescued exception

These new warnings help Elixir developers find bugs earlier and give more confidence when refactoring code, especially around maps and structs. While some of these warnings were emitted in the past, they were discovered using syntax analysis. The new warnings are more reliable, precise, and with better error messages. Keep in mind that not all maps have statically known keys, and the Elixir typechecker does not track types across all variables and function calls yet.

Future Elixir versions will continue inferring more types and type checking more constructs, bringing Elixir developers more warnings and quality of life improvements without changes to code. For more details, see our new [reference document on gradual set-theoretic types](https://hexdocs.pm/elixir/main/gradual-set-theoretic-types.html).

The type system was made possible thanks to a partnership between  [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The development work is currently sponsored by [Fresha](https://www.fresha.com/), [Starfish*](https://starfish.team/), and [Dashbit](https://dashbit.co/).

## Adding `Duration` and `shift/2` functions

TODO.

## v1.17.0-dev

### 1. Enhancements

#### Elixir

  * [Access] Add `Access.find/1` that mirrors `Enum.find/2`
  * [Date] Add `Date.shift/2` to shift dates with duration and calendar-specific semantics
  * [DateTime] Add `DateTime.shift/2` to shift datetimes with duration and calendar-specific semantics
  * [Duration] Add a new `Duration` data type
  * [GenServer] Add `c:GenServer.format_status/1` callback
  * [Kernel] Add `Kernel.get_in/1` with safe nil-handling for access and structs
  * [Kernel] Add `Kernel.is_non_struct_map/1` guard
  * [Kernel] Add `Kernel.to_timeout/1`
  * [Kernel] Emit warnings for undefined functions from modules defined within the same context as the caller code
  * [Macro] Add `Macro.Env.define_alias/4`, `Macro.Env.define_import/4`, `Macro.Env.define_require/4`, `Macro.Env.expand_alias/4`, `Macro.Env.expand_import/5`, and `Macro.Env.expand_require/6` to aid the implementation of language servers and embedded languages
  * [NaiveDateTime] Add `NaiveDateTime.shift/2` to shift naive datetimes with duration and calendar-specific semantics
  * [Process] Add `Process.set_label/1`
  * [String] Add `String.byte_slice/3` to slice a string to a maximum number of bytes while keeping it UTF-8 encoded
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
  * [mix test] Add `mix test --breakpoints` that sets up a breakpoint before each test that will run
  * [mix test] Add `mix test --repeat-until-failure` to rerun tests until a failure occurs
  * [mix test] Add `mix test --slowest-modules` to print slowest modules based on all of the tests they hold

### 2. Bug fixes

#### Elixir

  * [Code] Address a bug where AST nodes for `(a -> b)` were not wrapped as part of the literal encoder
  * [Kernel] Resolve inconsistencies of how `..` and `...` are handled at the AST level
  * [Kernel] Fix parsing precedence of ambiguous operators followed by containers

#### IEx

  * [IEx.Helpers] Update the history size whenever history is pruned

#### Mix

  * [mix deps] Fix error message for diverged SCM definition in sibling

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [GenServer] Deprecate `c:GenServer.format_status/2` callback to align with Erlang/OTP 25+

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
