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
  * [Mix.Tasks.Escript] Debug information is now stripped unless escript option `:strip_beam` is set to `false`

### 2. Bug fixes

#### Elixir

  * [Kernel] Support guards on anonymous functions of zero arity
  * [Kernel] Fix compilation of maps used as maps keys inside matches

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Kernel] `not left in right` is soft-deprecated in favor of `left not in right`

### 4. Deprecations

#### Elixir

  * [GenEvent] Hard deprecate `GenEvent` and provide alternatives in its docs
  * [Kernel] Using `()` to mean `nil` is deprecated
  * [Module] Using Erlang parse transforms via `@compile {:parse_transform, _}` is deprecated

## v1.4

The CHANGELOG for v1.4 releases can be found [in the v1.4 branch](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md).
