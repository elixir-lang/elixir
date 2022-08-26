# Changelog for Elixir v1.15

This release requires Erlang/OTP 24 and later.

## v1.15.0-dev

### 1. Enhancements

#### Elixir

  * [Code] `Code.format_string/2` now converts `'charlists'` into `~c"charlists"` by default
  * [Inspect] `Inspect` now renders `'charlists'` into `~c"charlists"` by default
  * [Kernel] Add `t:nonempty_binary/0` and `t:nonempty_bitstring/0`
  * [Kernel.CLI] Support `--sname undefined`/`--name undefined` so a name is automatically generated
  * [Process] Add `Process.alias/0,1` and `Process.unalias/1`
  * [Sets] Optimize many functions in `MapSet`
  * [Supervisor] Add support for automatic shutdown in `Supervisor`

#### Mix

  * [mix compile.app] Write `optional_applications` to .app file
  * [mix deps.get] Automatically install Hex and Rebar on `mix deps.get`/`mix deps.update`
  * [mix format] Allow multiple formatters per file extension and sigil

### 2. Bug fixes

  * [Kernel.ParallelCompiler] Make sure compiler doesn't crash when there are stray messages in the inbox

### 3. Soft deprecations (no warnings emitted)

### 4. Hard deprecations

#### Elixir

  * [Calendar] `Calendar.ISO.day_of_week/3` is deprecated in favor of `Calendar.ISO.day_of_week/4`
  * [Exception] `Exception.exception?/1` is deprecated in favor of `Kernel.is_exception/1`
  * [Kernel] Deprecate `...` as a valid function call identifier
  * [Regex] `Regex.regex?/1` is deprecated in favor of `Kernel.is_struct/2`

#### Logger

  * [Logger] `Logger.warn/2` is deprecated in favor of `Logger.warning/2`

## v1.14

The CHANGELOG for v1.14 releases can be found [in the v1.14 branch](https://github.com/elixir-lang/elixir/blob/v1.14/CHANGELOG.md).
