# Changelog for Elixir v1.14

## v1.14.0-dev

### 1. Enhancements

#### Elixir

  * [Code] Emit deprecation and type warnings on `Code.compile_string/2` and `Code.compile_quoted/2`
  * [Code] Warn if an outdated lexical tracker is given on eval
  * [Inspect] Improve error reporting when there is a faulty inspect implementation
  * [Kernel] Print escaped version of control chars when they show up as unexpected tokens
  * [Keyword] Add `Keyword.from_keys/2`
  * [Macro] Add `Macro.classify_atom/1` and `Macro.inspect_atom/1`
  * [Map] Add `Map.from_keys/2`
  * [Registry] Add `Registry.count_select/2`
  * [Stream] Add `Stream.duplicate/2`
  * [Version] Add `Version.to_string/1`
  * [Version] Colorize `Version.Requirement` source in Inspect protocol

#### Mix

  * [mix test] Improve error message when suite fails due to coverage

### 2. Bug fixes

#### IEx

  * [IEx] Disallow short-hand pipe after matches

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Application] Calling `Application.get_env/3` and friends in the module body is now discouraged, use `Application.compile_env/3` instead
  * [Bitwise] `use Bitwise` is deprecated, use `import Bitwise` instead
  * [Bitwise] `~~~` is deprecated in favor of `bnot` for clarity
  * [Kernel.ParallelCompiler] Returning a list or two-element tuple from `:each_cycle` is deprecated, return a `{:compile | :runtime, modules, warnings}` tuple instead

#### Mix

  * [Mix] `Mix.Tasks.Xref.calls/1` is deprecated in favor of compilation tracers

## v1.13

The CHANGELOG for v1.13 releases can be found [in the v1.13 branch](https://github.com/elixir-lang/elixir/blob/v1.13/CHANGELOG.md).
