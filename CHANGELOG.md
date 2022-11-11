# Changelog for Elixir v1.15

This release requires Erlang/OTP 24 and later.

## v1.15.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Include source code snippets in syntax errors

#### Elixir

  * [Code] `Code.format_string!/2` now converts `'charlists'` into `~c"charlists"` by default
  * [Date] Add `Date.before?/2` and `Date.after?/2`
  * [DateTime] Add `DateTime.before?/2` and `DateTime.after?/2`
  * [Inspect] `Inspect` now renders `'charlists'` as `~c"charlists"` by default
  * [Kernel] Add `t:nonempty_binary/0` and `t:nonempty_bitstring/0`
  * [Kernel] Treat `@behaviour`s as runtime dependencies
  * [Kernel] Warn for nested calls without parens inside keywords
  * [Kernel.CLI] Support `--sname undefined`/`--name undefined` so a name is automatically generated
  * [Keyword] Add `Keyword.split_with/2`
  * [Map] Add `Map.split_with/2`
  * [MapSet] Add `MapSet.split_with/2`
  * [NaiveDateTime] Add `NaiveDateTime.before?/2` and `NaiveDateTime.after?/2`
  * [Process] Add `Process.alias/0,1` and `Process.unalias/1`
  * [Range] Add `Range.split/2`
  * [Sets] Optimize many functions in `MapSet`
  * [String] Update Unicode to version 15.0.0
  * [Supervisor] Add support for automatic shutdown in `Supervisor`
  * [Task] Remove head blocking on `Task.yield_many/2`
  * [Time] Add `Time.before?/2` and `Time.after?/2`

#### ExUnit

  * [ExUnit] Add more color configuration to ExUnit CLI formatter
  * [ExUnit.Doctest] Add `ExUnit.DocTest.doctest_file/2`

#### IEx

  * [IEx.Info] Implement protocol for `Range` and `DateTime`

#### Mix

  * [Mix.Project] Support `def cli` to unify all CLI defaults in a single place
  * [mix compile] Set `--all-warnings` by default
  * [mix compile.app] Write `optional_applications` to `.app` file
  * [mix deps.get] Automatically install Hex and Rebar on `mix deps.get`/`mix deps.update`
  * [mix deps.get] Support `--check-locked` which raises if changes to the lockfile are required
  * [mix format] Allow multiple formatters per file extension and sigil
  * [mix format] Show diffs whenever `--check-formatted` fails

### 2. Bug fixes

#### Elixir

  * [Code.Formatter] Fix a scenario where a keyword followed by parenthesis could go above the maximum line length
  * [Code.Formatter] Remove unecessary parens in nullary type funs
  * [File] Do not raise if there are file system race conditions in `File.cp/2`
  * [Kernel] Expand macros on the left side of -> in `try/rescue`
  * [Kernel.ParallelCompiler] Make sure compiler doesn't crash when there are stray messages in the inbox
  * [URI] Make sure `URI.merge/2` works accordingly with relative paths

#### ExUnit

  * [ExUnit] Do not merge context as tags inside the runner to reduce memory usage when emitting events to formatters
  * [ExUnit] Do not expand or collect vars from quote in ExUnit assertions

#### IEx

  * [IEx] Do not spawn a process to read IO. This fixes a bug where multiline paste stopped working
    whenever the input reader was killed
  * [IEx] Do not perform completion for prompts triggered during code evaluation

### 3. Soft deprecations (no warnings emitted)

#### Mix

  * [Mix.Project] `:preferred_cli_env` is deprecated in favor of `:preferred_envs` in `def cli`
  * [Mix.Project] `:preferred_cli_target` is deprecated in favor of `:preferred_targets` in `def cli`

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
