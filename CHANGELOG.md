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
  * [Keyword] Add `replace/3` and `replace!/3` for replacing an existing key
  * [List] `List.starts_with?/2`
  * [Macro] Introduce `Macro.generate_arguments/2`
  * [Map] Optimize `Map.merge/3` by choosing merge direction
  * [Map] Add `replace/3` and `replace!/3` for replacing an existing key
  * [Registry] Support ETS guard conditions in `Registry.match/3`

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

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Kernel] `not left in right` is soft-deprecated in favor of `left not in right`

### 4. Deprecations

#### Elixir

  * [Calendar] Deprecate `NaiveDateTime` and `DateTime` in `Date.to_iso8601/1`, `Date.to_erl/1`, `Time.to_iso8601/1` and `Time.to_erl/1` to avoid loss of precision
  * [GenEvent] Hard deprecate `GenEvent` and provide alternatives in its docs
  * [Kernel] Using `()` to mean `nil` is deprecated
  * [Kernel] `Atom.to_char_list/1`, `Float.to_char_list/1`, `Integer.to_char_list/1`, `Kernel.to_char_list/1`, `String.to_char_list/1` have been deprecated in favor of their `to_charlist/1` version. This aligns with the naming conventions in both Erlang and Elixir
  * [Kernel] `:as_char_lists value` in `Inspect.Opts.t/0` type, in favor of `:as_charlists`
  * [Kernel] `:char_lists` key in `Inspect.Opts.t/0` type, in favor of `:charlists`
  * [Module] Using Erlang parse transforms via `@compile {:parse_transform, _}` is deprecated
  * [Typespec] `char_list/0` type is deprecated in favor of `charlist/0`

## v1.4

The CHANGELOG for v1.4 releases can be found [in the v1.4 branch](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md).
