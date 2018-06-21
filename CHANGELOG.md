# Changelog for Elixir v1.7

## v1.7.0-dev

### 1. Enhancements

#### Elixir

  * [Calendar.ISO] Support negative dates in `Calendar.ISO`
  * [Calendar] Add `Calendar.months_in_year/1` callback
  * [Code] Add `Code.compile_file/2` that compiles files without leaving footprints on the system
  * [Code] Add `Code.purge_compiler_modules/0` that purges any compiler module left behind. This is useful for live systems dynamically compiling code
  * [Date] Add `Date.months_in_year/1` function
  * [DynamicSupervisor] Use the name of the `DynamicSupervisor` as the ID whenever possible
  * [Function] Add `Function` module with `capture/3`, `info/1` and `info/2` functions
  * [GenServer] Support the new `handle_continue/2` callback on Erlang/OTP 21 onwards
  * [IO.ANSI] Add cursor movement to `IO.ANSI`
  * [Kernel] Introduce `__STACKTRACE__` to retrieve the current stacktrace inside `catch`/`rescue` (this will be a requirement for Erlang/OTP 21+)
  * [Kernel] Raise on unsafe variables in order to allow us to better track unused variables
  * [Kernel] Warn when using `length` to check if a list is not empty on guards
  * [Kernel] Add hints on mismatched `do`/`end` and others pairs
  * [Kernel] Warn when comparing structs using the `>`, `<`, `>=` and `<=` operators
  * [Macro] Add `Macro.special_form?/2` and `Macro.operator?/2` that returns true if the given name/arity is a special form or operator respectively
  * [Macro.Env] Add `Macro.Env.vars/1` and `Macro.Env.has_var?/2` that gives access to environment data without accessing private fields
  * [Regex] Include endianness in the regex version. This allows regexes to be recompiled when an archive is installed in a system with a different endianness
  * [Registry] Add `Registry.count/1` and `Registry.count_match/4`
  * [String] Update to Unicode 11
  * [System] Use ISO 8601 in `System.build_info/0`

#### ExUnit

  * [ExUnit.Assertion] Print the arguments in error reports when asserting on a function call. For example, if `assert is_list(arg)` fails, the argument will be shown in the report
  * [ExUnit.Diff] Improve diffing of lists when one list is a subset of the other
  * [ExUnit.DocTest] Show colored diffs on failed doctests
  * [ExUnit.Formatter] Excluded tests, via the `--exclude` and `--only` flags, are now shown as "Excluded" in reports. Tests skipped via `@tag :skip` are now exclusively shown as "Skipped" and in yellow

#### IEx

  * [IEx.Helpers] Add `IEx.Helpers.use_if_available/2`

#### Logger

  * [Logger] Improve error messages on invalid inputs

#### Mix

  * [mix archive.install] Add support for the Hex organization via `--organization`
  * [mix compile] Improve support for external build tools such as `rebar`
  * [mix escript.install] Add support for the Hex organization via `--organization`
  * [mix local] Use ipv6 with auto fallback to ipv4 when downloading data
  * [mix profile] Allow all profiling tasks to run programatically
  * [mix test] Add `--failed` option that only runs previously failed tests
  * [mix test] Print coverage summary by default when the `--cover` flag is given
  * [Mix.Project] Add `Mix.Project.clear_deps_cache/0`
  * [Mix.Project] Add `Mix.Project.config_mtime/0` that caches the config mtime values to avoid filesystem access

### 2. Bug fixes

#### Elixir

  * [IO.ANSI.Docs] Fix table column alignment when converting docs to ANSI escapes
  * [Kernel] Raise on unsafe variables as some of the code emitted with unsafe variables would not correctly propagate variables or would disable tail call optimization semantics

#### ExUnit

  * [ExUnit.Case] Raise proper error if `@tag` and `@moduletag` are used before `use ExUnit.Case`
  * [ExUnit.Case] Raise proper error if `@describetag` is used outside of describe
  * [ExUnit.DocTest] Emit proper assertion error on doctests with invalid UTF-8

#### Mix

  * [mix compile] Properly track config files in umbrella projects and recompile when any relevant umbrella configuration changes

### 3. Soft-deprecations (no warnings emitted)

  * [Code] Deprecate `Code.load_file/2` in favor of `Code.compile_file/2`
  * [Code] Deprecate `Code.loaded_files/0` in favor of `Code.required_files/0`
  * [Code] Deprecate `Code.unload_files/1` in favor of `Code.unrequire_files/1`

### 4. Hard-deprecations

#### Elixir

  * [Enum] `Enum.chunk/2/3/4` is deprecated in favor of `Enum.chunk_every/2/3/4` - notice `chunk_every` does not discard incomplete chunks by default
  * [GenServer] Warn if `super` is used in any of the GenServer callbacks
  * [Kernel] `not left in right` is ambiguous and is deprecated in favor of `left not in right`
  * [Kernel] Warn on confusing operator sequences, such as `1+++1` meaning `1 ++ +1` or `........` meaning `... .. ...`
  * [OptionParser] Deprecate dynamic option parser mode that depended on atoms to be previously loaded and therefore behaved inconsistently
  * [Stream] `Stream.chunk/2/3/4` is deprecated in favor of `Stream.chunk_every/2/3/4` - notice `chunk_every` does not discard incomplete chunks by default

## v1.6

The CHANGELOG for v1.6 releases can be found [in the v1.6 branch](https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md).
