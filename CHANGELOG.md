# Changelog for Elixir v1.15

This release requires Erlang/OTP 24 and later.

## v1.15.0-dev

### 1. Enhancements

#### Elixir

  * [Code] `Code.format_string/2` now converts `'charlists'` into `~c"charlists"` by default
  * [Inspect] `Inspect` now renders `'charlists'` as `~c"charlists"` by default
  * [Kernel] Add `t:nonempty_binary/0` and `t:nonempty_bitstring/0`
  * [Kernel] Treat `@behaviour`s as runtime dependencies
  * [Kernel.CLI] Support `--sname undefined`/`--name undefined` so a name is automatically generated
  * [Keyword] Add `Keyword.split_with/2`
  * [Map] Add `Map.split_with/2`
  * [MapSet] Add `MapSet.split_with/2`
  * [Process] Add `Process.alias/0,1` and `Process.unalias/1`
  * [Sets] Optimize many functions in `MapSet`
  * [String] Update Unicode to version 15.0.0
  * [Supervisor] Add support for automatic shutdown in `Supervisor`

#### Mix

  * [mix compile.app] Write `optional_applications` to .app file
  * [mix deps.get] Automatically install Hex and Rebar on `mix deps.get`/`mix deps.update`
  * [mix format] Allow multiple formatters per file extension and sigil
  * [mix format] Show diffs whenever `--check-formatted` fails

### 2. Bug fixes

#### Elixir

  * [Kernel.ParallelCompiler] Make sure compiler doesn't crash when there are stray messages in the inbox

#### ExUnit

  * [ExUnit] Do not merge context as tags inside the runner to reduce memory usage when emitting events to formatters

#### IEx

  * [IEx] Do not spawn a process to read IO. This fixes a bug where multiline paste stopped working
    whenever the input reader was killed

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
