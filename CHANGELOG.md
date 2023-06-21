# Changelog for Elixir v1.16

## v1.16.0-dev

### 1. Enhancements

### 2. Bug fixes

#### Elixir

  * [IO] Raise when using `IO.binwrite/2` on terminated device (mirroring `IO.write/2`)
  * [Path] Ensure `Path.relative_to/2` returns a relative path when the given argument does not share a common prefix with `cwd`

### 3. Soft deprecations (no warnings emitted)

  * [Path] Deprecate `Path.safe_relative_to/2` in favor of `Path.safe_relative/2`

### 4. Hard deprecations

#### Elixir

  * [Date] Deprecate inferring a range with negative step, call `Date.range/3` with a negative step instead
  * [Enum] Deprecate passing a range with negative step on `Enum.slice/2`, give `first..last//1` instead
  * [String] Deprecate passing a range with negative step on `String.slice/2`, give `first..last//1` instead

#### ExUnit

  * [ExUnit.Formatter] Deprecate `format_time/2`, use `format_times/1` instead

#### Mix

  * [mix compile.leex] Require `:leex` to be added as a compiler to run the `leex` compiler
  * [mix compile.yecc] Require `:yecc` to be added as a compiler to run the `yecc` compiler

## v1.15

The CHANGELOG for v1.15 releases can be found [in the v1.15 branch](https://github.com/elixir-lang/elixir/blob/v1.15/CHANGELOG.md).
