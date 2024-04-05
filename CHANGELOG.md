# Changelog for Elixir v1.17

## Adding `Duration` and `shift/2` functions

TODO.

## v1.17.0-dev

### 1. Enhancements

#### Elixir

  * [Access] Add `Access.find/1` that mirrors `Enum.find/2`
  * [Date] Add `Date.shift/2` to shift dates with duration and calendar-specific semantics
  * [DateTime] Add `DateTime.shift/2` to shift datetimes with duration and calendar-specific semantics
  * [Duration] Add a new `Duration` data type
  * [Kernel] Add `Kernel.get_in/1` with safe nil-handling for access and structs
  * [Kernel] Emit warnings for undefined functions from modules defined within the same context as the caller code
  * [Macro] Add `Macro.Env.define_alias/4`, `Macro.Env.define_import/4`, `Macro.Env.define_require/4`, `Macro.Env.expand_alias/4`, `Macro.Env.expand_import/5`, and `Macro.Env.expand_require/6` to aid the implementation of language servers and embeddeed languages
  * [NaiveDateTime] Add `NaiveDateTime.shift/2` to shift naive datetimes with duration and calendar-specific semantics
  * [Process] Add `Process.set_label/1`
  * [String] Add `String.byte_slice/3` to slice a string to a maximum number of bytes while keeping it UTF-8 encoded
  * [Time] Add `Time.shift/2` to shift times with duration and calendar-specific semantics

#### ExUnit

  * [ExUnit] Propagate the test process itself as a caller in `start_supervised`

#### IEx

  * [IEx.Helpers] Warns if `recompile` was called and the current working directory changed
  * [IEx.Helpers] Add `c/0` as an alias to `continue/0`
  * [IEx.Pry] Add `IEx.Pry.annotated_quoted/3` to annotate a quoted expression with pry breakpoints

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
  * [Kernel] Fix parsing of ambiguous operators followed by containers

#### IEx

  * [IEx.Helpers] Also update the history size whenever it is pruned

### 3. Soft deprecations (no warnings emitted)

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
