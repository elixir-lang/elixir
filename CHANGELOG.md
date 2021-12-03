# Changelog for Elixir v1.14

## v1.14.0-dev

### 1. Enhancements

#### Elixir

  * [Code] Emit deprecation and type warnings on `Code.compile_string/2` and `Code.compile_quoted/2`
  * [Code] Warn if an outdated lexical tracker is given on eval
  * [Inspect] Improve error reporting when there is a faulty inspect implementation
  * [Kernel] Print escaped version of control chars when they show up as unexpected tokens
  * [Keyword] Add `Keyword.from_keys/2`
  * [Map] Add `Map.from_keys/2`
  * [Registry] Add `count_select/2`
  * [Version] Add `Version.to_string/1`

### 2. Bug fixes

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Application] Calling `Application.get_env/3` and friends in the module body is now discouraged, use `Application.compile_env/3` instead
  * [Kernel.ParallelCompiler] Returning a list or two-element tuple from `:each_cycle` is deprecated, return a `{:compile | :runtime, modules, warnings}` tuple instead

#### Mix

  * [Mix] `Mix.Tasks.Xref.calls/1` is deprecated in favor of compilation tracers

## v1.13

The CHANGELOG for v1.13 releases can be found [in the v1.13 branch](https://github.com/elixir-lang/elixir/blob/v1.13/CHANGELOG.md).
