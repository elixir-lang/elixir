# Changelog for Elixir v1.17

## v1.17.0-dev

### 1. Enhancements

#### Elixir

  * [Kernel] Add `Kernel.get_in/1` with safe nil-handling for access and structs
  * [String] Add `String.byte_slice/3` to slice a string to a maximum number of bytes while keeping it UTF-8 encoded

#### ExUnit

  * [ExUnit] Propagate the test process itself as a caller in `start_supervised`

#### IEx

  * [IEx.Helpers] Warns if `recompile` was called and the current working directory changed
  * [IEx.Helpers] Add `c/0` as an alias to `continue/0`
  * [IEx.Pry] Add `IEx.Pry.annotated_quoted/3` to annotate a quoted expression with pry breakpoints

#### Mix

  * [mix test] Add `mix test --breakpoints` that sets up a breakpoint before each test that will run

### 2. Bug fixes

#### Elixir

  * [Code] Address a bug where AST nodes for `(a -> b)` were not wrapper as part of the literal encoder
  * [Kernel] Resolve inconsistencies of how `..` and `...` are handled at the AST level

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
