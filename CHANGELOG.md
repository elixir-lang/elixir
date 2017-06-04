# Changelog for Elixir v1.5

## v1.5.0-dev

### 1. Enhancements

#### Elixir

  * [Access] Optimize Access.get/2
  * [Calendar] Limit `Calendar.ISO` up to year 10000
  * [Calendar] Add Rata Die format for conversions between Calendars and `Date.convert/2`, `Time.convert/2`, `NaiveDateTime.convert/2` and `DateTime.convert/2` (as well as bang variants)
  * [Calendar] Add `:calendar` field to `Time` struct
  * [Calendar] Add `Time.diff/3`, `Date.add/2`, `Date.diff/2`, `DateTime.diff/3`
  * [Enum] Add `Enum.chunk_by/4` and `Stream.chunk_by/4`
  * [Exception] Add `Exception.blame/3` that adds metadata to exceptions
  * [File] Add `File.read_link/1` and `File.read_link!/1`
  * [File] Introduce `:trim_bom` option for `File.stream!/2`
  * [Inspect] Add `:printable_limit` to control the limit of printable structures
  * [Integer] Add `Integer.gcd/2`
  * [Kernel] Use the new `debug_info` chunk in OTP 20. This provides a mechanism for tools to retrieve the Elixir AST from beam files
  * [Kernel] `defoverridable/1` accepts a module name as argument and marks all callbacks as overridable
  * [Kernel] Allow non-quoted Unicode atoms and variables according to Unicode Annex #31 (see Unicode Syntax document)
  * [Kernel] Warn when a :__struct__ key is used when building/updating structs
  * [Keyword] Add `replace/3` and `replace!/3` for replacing an existing key
  * [List] `List.starts_with?/2`
  * [Macro] Introduce `Macro.generate_arguments/2`
  * [Map] Optimize `Map.merge/3` by choosing merge direction
  * [Map] Add `replace/3` and `replace!/3` for replacing an existing key
  * [MapSet] Reduce `MapSet` size when serialized to approximately half
  * [Process] Add `Process.cancel_timer/2`
  * [Registry] Support ETS guard conditions in `Registry.match/3`
  * [Task] Support `:on_timeout` in `Task.async_stream` to control how tasks are terminated

#### ExUnit

  * [ExUnit] Show code snippet from test source file in case of test errors
  * [ExUnit] Show the value of variables used in an assertion
  * [ExUnit] Use `Exception.blame/3` when formatting test errors

#### IEx

  * [IEx.Evaluator] Use `Exception.blame/3` when showing errors in the terminal
  * [IEx.Helpers] Add `e/1` IEx helper to list all exports in a module
  * [IEx.Info] Implement `IEx.Info` protocol for calendar types

#### Logger

  * [Logger] Add `metadata: :all` configuration to log all metadata

#### Mix

  * [mix escript.build] Strip debug information from escripts by default and add option `:strip_beam` which defaults to true
  * [mix loadpaths] Ensure `--no-deps-check` do not trigger SCM callbacks (such as `git`)
  * [mix local.hex] Add `--if-missing` flag to `local.hex` mix task
  * [mix profile.cprof] Add `Mix.Tasks.Profile.Cprof` for count-based profiling

### 2. Bug fixes

#### Elixir

  * [File] Support `:ram`/`:raw` files in `File.copy/2`
  * [Kernel] Support guards on anonymous functions of zero arity
  * [Kernel] Fix compilation of maps used as maps keys inside matches
  * [Record] Properly escape quoted expressions passed to `defrecord`
  * [String] Consider Unicode non-characters valid according to the specification in `String.valid?/1`

#### ExUnit

  * [ExUnit] Properly account failed tests when `setup_all` fails

#### IEx

  * [IEx] Skip autocompletion of module names that are invalid without being quoted
  * [IEx] Do not start oldshell alongside IEx

#### Mix

  * [mix compile.elixir] Store multiple sources in case of module conflicts. This solves an issue where `_build` would get corrupted when compiling Elixir projects with module conflicts

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Kernel] `not left in right` is soft-deprecated in favor of `left not in right`

### 4. Deprecations

#### Elixir

  * `Atom.to_char_list/1`, `Float.to_char_list/1`, `Integer.to_char_list/1`, `Integer.to_char_list/2`, `Kernel.to_char_list/1`, `List.Chars.to_char_list/1`, `String.to_char_list/1` have been deprecated in favor of their `to_charlist` version. This aligns with the naming conventions in both Erlang and Elixir
  * [Enum] Deprecate `Enum.filter_map/3` in favor of `Enum.filter/2` + `Enum.map/2` or for-comprehensions
  * [GenEvent] Deprecate `GenEvent` and provide alternatives in its docs
  * [Kernel] Using `()` to mean `nil` is deprecated
  * [Kernel] `:as_char_lists value` in `Inspect.Opts.t/0` type, in favor of `:as_charlists`
  * [Kernel] `:char_lists` key in `Inspect.Opts.t/0` type, in favor of `:charlists`
  * [Module] Using Erlang parse transforms via `@compile {:parse_transform, _}` is deprecated
  * [Stream] Deprecate `Stream.filter_map/3` in favor of `Stream.filter/2` + `Stream.map/2`
  * [String] `String.ljust/3` and `String.rjust/3` are deprecated in favor of `String.pad_leading/3` and `String.pad_trailing/3` with a binary padding
  * [String] `String.strip/1` and `String.strip/2` are deprecated in favor of `String.trim/1` and `String.trim/2`
  * [String] `String.lstrip/1` and `String.rstrip/1` are deprecated in favor of `String.trim_leading/1` and `String.trim_trailing/1`
  * [String] `String.lstrip/2` and `String.rstrip/2` are deprecated in favor of `String.trim_leading/2` and `String.trim_trailing/2` with a binary as second argument
  * [Typespec] `char_list/0` type is deprecated in favor of `charlist/0`

## v1.4

The CHANGELOG for v1.4 releases can be found [in the v1.4 branch](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md).
