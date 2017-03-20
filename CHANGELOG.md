# Changelog for Elixir v1.5

## v1.5.0-dev

### 1. Enhancements

#### Elixir

  * [File] Add `File.read_link/1` and `File.read_link!/1`
  * [File] Introduce `:trim_bom` option for `File.stream!/2`
  * [List] `List.starts_with?/2`
  * [Macro] Introduce `Macro.generate_arguments/2`

#### IEx

  * [IEx.Helpers] Add `e/1` IEx helper to list all exports in a module
  * [IEx.Info] Implement `IEx.Info` protocol for calendar types

#### Mix

  * [Mix.Hex] Add `--if-missing` flag to `local.hex` mix task

### 2. Bug fixes

#### Elixir

  * [Kernel] Support guards on anonymous functions of zero arity
  * [Kernel] Fix compilation of maps used as maps keys inside matches

### 3. Soft deprecations (no warnings emitted)

#### Elixir
  * [Kernel] `not left in right` is soft-deprecated in favor of `left not in right`

### 4. Deprecations

#### Elixir

  * [Calendar] Calling `Date.to_erl/1`, `Date.to_iso8601/1`, `Time.to_erl/1` and `Time.to_iso8601/1` directly with a `DateTime` or `NaiveDateTime` struct is deprecated in favour of converting them to `Date`s/`Time`s first by using `DateTime.to_date/1`, `NaiveDateTime.to_date/1`, `DateTime.to_time/1` and `NaiveDateTime.to_time/1` respectively.
  * [GenEvent] Hard deprecate `GenEvent` and provide alternatives in its docs
  * [Kernel] Using `()` to mean `nil` is deprecated
  * [Kernel] `Atom.to_char_list/1`, `Float.to_char_list/1`, `Integer.to_char_list/1`, `Kernel.to_char_list/1`, `String.to_char_list/1` have been deprecated in favor of their `to_charlist/1` version. This aligns with the naming conventions in both Erlang and Elixir
  * [Kernel] `:as_char_lists value` in `Inspect.Opts.t/0` type, in favor of `:as_charlists`
  * [Kernel] `:char_lists` key in `Inspect.Opts.t/0` type, in favor of `:charlists`
  * [Module] Using Erlang parse transforms via `@compile {:parse_transform, _}` is deprecated
  * [Typespec] `char_list/0` type is deprecated in favor of `charlist/0`

## v1.4

The CHANGELOG for v1.4 releases can be found [in the v1.4 branch](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md).
