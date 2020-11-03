# Changelog for Elixir v1.12

## v1.12.0-dev

### 1. Enhancements

#### Elixir

  * [Enum] Add `Enum.zip_with/2` and `Enum.zip_with/3`
  * [List] Add default value for `List.first/1` and `List.last/1`
  * [Kernel] Do not add runtime dependencies to remotes in typespecs
  * [Stream] Add `Stream.zip_with/2` and `Stream.zip_with/3`
  * [String] Add `:turkic` mode option to String case functions

#### ExUnit

  * [ExUnit] Intercept SIGQUIT and show a list of all aborted tests as well as intermediate test results

#### IEx

  * [IEx] Make IEx' parser configurable to allow special commands
  * [IEx] Show function signature when pressing tab after the opening parens of a function

### 2. Bug fixes

#### Elixir

  * [Kernel] Public functions without documentation now appear as an empty map on `Code.fetch_docs/1`, unless they start with underscore, where they remain as `:none`. This aligns Elixir's implementation with EEP48

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Kernel] Deprecate `@foo()` in favor of `@foo`
  * [System] Deprecate `System.stacktrace/0` (it was already deprecated outside of catch/rescue and now it is deprecated everywhere)

#### Mix

  * [mix compile] The `:xref` compiler is deprecated and it has no effect. Please remove it from your mix.exs file.

## v1.11

The CHANGELOG for v1.11 releases can be found [in the v1.11 branch](https://github.com/elixir-lang/elixir/blob/v1.11/CHANGELOG.md).
