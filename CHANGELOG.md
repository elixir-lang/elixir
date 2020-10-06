# Changelog for Elixir v1.12

## v1.12.0-dev

### 1. Enhancements

#### Elixir

  * [List] Add default value for `List.first/1` and `List.last/1`
  * [String] Add `:turkic` mode option to String case functions

#### ExUnit

  * [ExUnit] Intercept SIGQUIT and show a list of all aborted tests as well as intermediate test results

#### IEx

  * [IEx] Make IEx' parser to be configurable to allow special commands

### 2. Bug fixes

  * [Kernel] Public functions without documentation now appear as an empty map on `Code.fetch_docs/1`, unless they start with underscore, where they remain as `:none`. This aligns Elixir's implementation with EEP48

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Kernel] Deprecate `@foo()` in favor of `@foo`

#### Mix

  * [mix compile] The `:xref` compiler is deprecated and it has no effect. Please remove it from your mix.exs file.

## v1.11

The CHANGELOG for v1.11 releases can be found [in the v1.11 branch](https://github.com/elixir-lang/elixir/blob/v1.11/CHANGELOG.md).
