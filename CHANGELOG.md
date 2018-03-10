# Changelog for Elixir v1.7

## v1.7.0-dev

### 1. Enhancements

#### Elixir

  * [Calendar.ISO] Support negative dates in `Calendar.ISO`
  * [Calendar] Add `months_in_year/1` callback
  * [Code] Add `Code.compile_file/2` that compiles files without leaving footprints on the system
  * [Date] Add `months_in_year/1` function
  * [IO.ANSI] Add cursor movement to `IO.ANSI`
  * [Kernel] Introduce `__STACKTRACE__` to retrieve the current stacktrace inside `catch`/`rescue` (this will be a requirement for Erlang/OTP 21+)
  * [Kernel] Raise on unsafe variables in order to allow us to better track unused variables
  * [Macro] Add `Macro.special_form?/2` and `Macro.operator?/2` that returns true if the given name/arity is a special form or operator respectively
  * [Macro.Env] Add `Macro.Env.vars/1` and `Macro.Env.has_var?/2` that gives access to environment data without accessing private fields
  * [System] Use ISO 8601 in `System.build_info/0`

#### ExUnit

  * [ExUnit.Assertion] Print the arguments in error reports when asserting on a function call. For example, if `assert is_list(arg)` fails, the argument will be shown in the report
  * [ExUnit.Formatter] Excluded tests, via the `--exclude` and `--only` flags, are now shown as "Excluded" in reports. Tests skipped via `@tag :skip` are now exclusively shown as "Skipped" and in yellow

#### IEx

  * [IEx.Helpers] Add `IEx.Helpers.use_if_available/2`

#### Mix

  * [mix test] Add `--failed` option that only runs previously failed tests
  * [Mix.Project] Add `Mix.Project.clear_deps_cache/0`

### 2. Bug fixes

#### Elixir

  * [IO.ANSI.Docs] Fix table column alignment when converting docs to ANSI escapes
  * [Kernel] Raise on unsafe variables as some of the code emitted with unsafe variables would not correctly propagate variables or would disable tail call optimization semantics

### 3. Soft deprecations (no warnings emitted)

  * [Code] Deprecate `Code.load_file/2` in favor of `Code.compile_file/2`
  * [Code] Deprecate `Code.loaded_files/0` in favor of `Code.required_files/0`
  * [Code] Deprecate `Code.unload_files/1` in favor of `Code.unrequire_files/1`

### 4. Deprecations

#### Elixir

  * [Enum] `Enum.chunk/2/3/4` is deprecated in favor of `Enum.chunk_every/2/3/4` - notice `chunk_every` does not discard incomplete chunks by default
  * [Kernel] `not left in right` is ambiguous and is deprecated in favor of `left not in right`
  * [Kernel] Warn on confusing operator sequences, such as `1+++1` meaning `1 ++ +1` or `........` meaning `... .. ...`
  * [OptionParser] Deprecate dynamic option parser mode that depended on atoms to be previously loaded and therefore behaved inconsistently
  * [Stream] `Stream.chunk/2/3/4` is deprecated in favor of `Stream.chunk_every/2/3/4` - notice `chunk_every` does not discard incomplete chunks by default

## v1.6

The CHANGELOG for v1.6 releases can be found [in the v1.6 branch](https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md).
