# Changelog for Elixir v1.5

## v1.5.0-dev

### 1. Enhancements

#### Elixir

  * [Calendar] Limit `Calendar.ISO` up to year 10000
  * [Calendar] Add Rata Die format for conversions between Calendars and `Date.convert/2`, `Time.convert/2`, `NaiveDateTime.convert/2` and `DateTime.convert/2` (as well as bang variants)
  * [Calendar] Add `:calendar` field to `Time` struct
  * [Calendar] Add `Date.diff/2`
  * [File] Add `File.read_link/1` and `File.read_link!/1`
  * [File] Introduce `:trim_bom` option for `File.stream!/2`
  * [Integer] Add `Integer.gcd/2`
  * [Kernel] Use the new `debug_info` chunk in OTP 20. This provides a mechanism for tools to retrieve the Elixir AST from beam files
  * [Kernel] `defoverridable/1` accepts a module name as argument and marks all callbacks as overridable
  * [Keyword] Add `replace/3` and `replace!/3` for replacing an existing key
  * [List] `List.starts_with?/2`
  * [Macro] Introduce `Macro.generate_arguments/2`
  * [Map] Optimize `Map.merge/3` by choosing merge direction
  * [Map] Add `replace/3` and `replace!/3` for replacing an existing key
  * [MapSet] Reduce `MapSet` size when serialized to approximately half
  * [Process] Add `Process.cancel_timer/2`
  * [Registry] Support ETS guard conditions in `Registry.match/3`
  * [Task] Support `:on_timeout` in `Task.async_stream` to control how tasks are terminated

#### IEx

  * [IEx.Helpers] Add `e/1` IEx helper to list all exports in a module
  * [IEx.Info] Implement `IEx.Info` protocol for calendar types

#### Logger

  * [Logger] Add `metadata: :all` option

#### Mix

  * [Mix.Hex] Add `--if-missing` flag to `local.hex` mix task
  * [Mix.Tasks] Strip debug information from escripts by default and add option `:strip_beam` which defaults to true
  * [Mix.Tasks] Add `Mix.Tasks.Profile.Cprof` for count-based profiling

### 2. Bug fixes

#### Elixir

  * [Kernel] Support guards on anonymous functions of zero arity
  * [Kernel] Fix compilation of maps used as maps keys inside matches

#### ExUnit

  * [ExUnit] Properly account failed tests when `setup_all` fails

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Kernel] `not left in right` is soft-deprecated in favor of `left not in right`

### 4. Deprecations

#### Elixir

  * `Atom.to_char_list/1`, `Float.to_char_list/1`, `Integer.to_char_list/1`, `Integer.to_char_list/2`, `Kernel.to_char_list/1`, `List.Chars.to_char_list/1`, `String.to_char_list/1` have been deprecated in favor of their `to_charlist` version. This aligns with the naming conventions in both Erlang and Elixir
  * [Calendar] Deprecate `NaiveDateTime` and `DateTime` in `Date.to_iso8601/1`, `Date.to_erl/1`, `Time.to_iso8601/1` and `Time.to_erl/1` to avoid loss of precision
  * [GenEvent] Deprecate `GenEvent` and provide alternatives in its docs
  * [Kernel] Using `()` to mean `nil` is deprecated
  * [Kernel] `:as_char_lists value` in `Inspect.Opts.t/0` type, in favor of `:as_charlists`
  * [Kernel] `:char_lists` key in `Inspect.Opts.t/0` type, in favor of `:charlists`
  * [Module] Using Erlang parse transforms via `@compile {:parse_transform, _}` is deprecated
  * [String] `String.ljust/3` and `String.rjust/3` are deprecated in favor of `String.pad_leading/3` and `String.pad_trailing/3` with a binary padding
  * [String] `String.strip/1` and `String.strip/2` are deprecated in favor of `String.trim/1` and `String.trim/2`
  * [String] `String.lstrip/1` and `String.rstrip/1` are deprecated in favor of `String.trim_leading/1` and `String.trim_trailing/1`
  * [String] `String.lstrip/2` and `String.rstrip/2` are deprecated in favor of `String.trim_leading/2` and `String.trim_trailing/2` with a binary as second argument
  * [Typespec] `char_list/0` type is deprecated in favor of `charlist/0`

## v1.4

The CHANGELOG for v1.4 releases can be found [in the v1.4 branch](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md).
