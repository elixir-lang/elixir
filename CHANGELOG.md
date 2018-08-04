# Changelog for Elixir v1.7

Elixir v1.7 is the last release to support Erlang/OTP 19. We recommend everyone to migrate to Erlang/OTP 20+.

## Documentation metadata

Elixir v1.7 implements [EEP 48](http://erlang.org/eep/eeps/eep-0048.html). EEP 48 aims to bring documentation interoperability across all languages running on the Erlang VM. The documentation format proposed by EEP 48 also supports metadata, which is now fully exposed to Elixir developers:

```elixir
@moduledoc "A brand new module"
@moduledoc authors: ["Jane", "Mary"], since: "1.4.0"
```

Passing metadata is supported on `@doc`, `@moduledoc` and `@typedoc`.

To access the new documentation format, developers should use `Code.fetch_docs/1`. The old documentation format is no longer available and the old `Code.get_docs/2` function will return `nil` accordingly.

Tools like IEx and ExDoc have been updated to leverage the new format and show relevant metadata to users. While Elixir allows any metadata to be given, those tools currently exhibit only `:deprecated` and `:since`. Other keys may be shown in the future.

## The `__STACKTRACE__` construct

Erlang/OTP 21.0 introduces a new way to retrieve the stacktrace that is lexically scoped and no longer relies on side-effects like `System.stacktrace/0` does. Before one would write:

```elixir
try do
  ... something that may fail ...
rescue
  e ->
    log(e, System.stacktrace())
    reraise(e, System.stacktrace())
end
```

In Elixir v1.7, this can be written as:

```elixir
try do
  ... something that may fail ...
rescue
  e ->
    log(e, __STACKTRACE__)
    reraise(e, __STACKTRACE__)
end
```

This change may also yield performance improvements in the future, since the lexical scope allows us to track precisely when a stacktrace is used and we no longer need to keep references to stacktrace entries after the `try` construct finishes.

Other parts of the exception system have been improved. For example, more information is provided in certain occurrences of `ArgumentError`, `ArithmeticError` and `KeyError` messages.

## Erlang/OTP logger integration

Erlang/OTP 21 includes a new `:logger` module. Elixir v1.7 fully integrates with the new `:logger` and leverages its metadata system. The `Logger.Translator` mechanism has also been improved to export metadata, allowing custom Logger backends to leverage information such as:

  * `:crash_reason` - a two-element tuple with the throw/error/exit reason as first argument and the stacktrace as second

  * `:initial_call` - the initial call that started the process

  * `:registered_name` - the process registered name as an atom

We recommend Elixir libraries that previously hooked into Erlang's `:error_logger` to hook into `Logger` instead, in order to support all current and future Erlang/OTP versions.

## Other Logger improvements

Previously, Logger macros such as `debug`, `info`, and so on would always evaluate their arguments, even when nothing would be logged. From Elixir v1.7, the arguments are only evaluated when the message is logged.

The Logger configuration system also accepts a new option called `:compile_time_purge_matching` that allows you to remove log calls with specific compile-time metadata. For example, to remove all logger calls from application `:foo` with level lower than `:info`, as well as remove all logger calls from `Bar.foo/3`, you can use the following configuration:

```elixir
config :logger,
  compile_time_purge_matching: [
    [application: :foo, level_lower_than: :info],
    [module: Bar, function: "foo/3"]
  ]
```

## ExUnit improvements

ExUnit has also seen its own share of improvements. Assertions such as `assert some_fun(arg1, arg2, arg3)` will now include the value of each argument in the failure report:

```
  1) test function call arguments (TestOneOfEach)
     lib/ex_unit/examples/one_of_each.exs:157
     Expected truthy, got false
     code: assert some_vars(1 + 2, 3 + 4)
     arguments:

         # 1
         3

         # 2
         7

     stacktrace:
       lib/ex_unit/examples/one_of_each.exs:158: (test)
```

Furthermore, failures in doctests are now colored and diffed.

On the `mix test` side of things, there is a new `--failed` flag that runs all tests that failed the last time they ran. Finally, coverage reports generated with `mix test --cover` include a summary out of the box:

```
Generating cover results ...

Percentage | Module
-----------|--------------------------
   100.00% | Plug.Exception.Any
   100.00% | Plug.Adapters.Cowboy2.Stream
   100.00% | Collectable.Plug.Conn
   100.00% | Plug.Crypto.KeyGenerator
   100.00% | Plug.Parsers
   100.00% | Plug.Head
   100.00% | Plug.Router.Utils
   100.00% | Plug.RequestId
       ... | ...
-----------|--------------------------
    77.19% | Total
```

## v1.7.2 (2018-08-05)

### 1. Bug fixes

#### Elixir

  * [DateTime] Take negative years into account in `DateTime.from_iso8601/1`
  * [Kernel] Do not emit warnings for repeated docs over different clauses due to false negatives

#### Mix

  * [mix compile] Properly mark top-level dependencies as optional and as runtime. This fixes a bug where Mix attempted to start optional dependencies of a package when those optional dependencies were not available
  * [mix compile] Avoid deadlock when a timestamp later than current time is found
  * [mix help] Show task and alias help when both are available
  * [mix test] Do not fail suite if there are no tests to run


## v1.7.1 (2018-07-26)

### 1. Bug fixes

#### Elixir

  * [Calendar] Work-around a Dialyzer bug that causes it to loop for a long time, potentially indefinitely

## v1.7.0 (2018-07-25)

### 1. Enhancements

#### Elixir

  * [Calendar.ISO] Support negative dates in `Calendar.ISO`
  * [Calendar] Add `Calendar.months_in_year/1` callback
  * [Code] Add `Code.compile_file/2` that compiles files without leaving footprints on the system
  * [Code] Add `Code.purge_compiler_modules/0` that purges any compiler module left behind. This is useful for live systems dynamically compiling code
  * [Code] Add `Code.fetch_docs/1` that returns docs in the [EEP 48](http://erlang.org/eep/eeps/eep-0048.html) format
  * [Date] Add `Date.months_in_year/1` function
  * [DynamicSupervisor] Use the name of the `DynamicSupervisor` as the ID whenever possible
  * [Exception] Provide "did you mean" suggestions on KeyError
  * [Exception] Provide more information on ArithmeticError on Erlang/OTP 21+
  * [Function] Add `Function` module with `capture/3`, `info/1` and `info/2` functions
  * [GenServer] Support the new `handle_continue/2` callback on Erlang/OTP 21+
  * [IO.ANSI] Add cursor movement to `IO.ANSI`
  * [Kernel] Support adding arbitrary documentation metadata by passing a keyword list to `@doc`, `@moduledoc` and `@typedoc`
  * [Kernel] Introduce `__STACKTRACE__` to retrieve the current stacktrace inside `catch`/`rescue` (this will be a requirement for Erlang/OTP 21+)
  * [Kernel] Raise on unsafe variables in order to allow us to better track unused variables
  * [Kernel] Warn when using `length` to check if a list is not empty on guards
  * [Kernel] Add hints on mismatched `do`/`end` and others pairs
  * [Kernel] Warn when comparing structs using the `>`, `<`, `>=` and `<=` operators
  * [Kernel] Warn on unsupported nested comparisons such as `x < y < z`
  * [Kernel] Warn if redefining documentation across clauses of the same definition
  * [Kernel] Warn on unnecessary quotes around atoms, keywords and calls
  * [Macro] Add `Macro.special_form?/2` and `Macro.operator?/2` that returns `true` if the given name/arity is a special form or operator respectively
  * [Macro.Env] Add `Macro.Env.vars/1` and `Macro.Env.has_var?/2` that gives access to environment data without accessing private fields
  * [Regex] Include endianness in the regex version. This allows regexes to be recompiled when an archive is installed in a system with a different endianness
  * [Registry] Add `Registry.count/1` and `Registry.count_match/4`
  * [String] Update to Unicode 11
  * [StringIO] Add `StringIO.open/3`
  * [System] Use ISO 8601 in `System.build_info/0`

#### ExUnit

  * [ExUnit.Assertion] Print the arguments in error reports when asserting on a function call. For example, if `assert is_list(arg)` fails, the argument will be shown in the report
  * [ExUnit.Diff] Improve diffing of lists when one list is a subset of the other
  * [ExUnit.DocTest] Show colored diffs on failed doctests
  * [ExUnit.Formatter] Excluded tests, via the `--exclude` and `--only` flags, are now shown as "Excluded" in reports. Tests skipped via `@tag :skip` are now exclusively shown as "Skipped" and in yellow

#### IEx

  * [IEx.Helpers] Add `use_if_available/2`
  * [IEx.Helpers] Allow `force: true` option in `recompile/1`
  * [IEx.Helpers] Add `:allocators` pane to `runtime_info/1`
  * [IEx.Helpers] Show documentation metadata in `h/1` helpers

#### Logger

  * [Logger] Ensure nil metadata is always pruned
  * [Logger] Only evaluate Logger macro arguments when the message will be logged
  * [Logger] Add `:compile_time_purge_matching` to purge logger calls that match certain compile time metadata, such as module names and application names
  * [Logger] Log to `:stderr` if a backend fails and there are no other backends
  * [Logger] Allow translators to return custom metadata
  * [Logger] Return `:crash_reason`, `:initial_call` and `:registered_name` as metadata in crash reports coming from Erlang/OTP

#### Mix

  * [mix archive.install] Add support for the Hex organization via `--organization`
  * [mix archive.uninstall] Support `--force` flag
  * [mix compile] Improve support for external build tools such as `rebar`
  * [mix deps] Include `override: true` in rebar dependencies to make the behaviour closer to how rebar3 works (although diverged deps are still marked as diverged)
  * [mix escript.install] Add support for the Hex organization via `--organization`
  * [mix escript.uninstall] Support `--force` flag
  * [mix help] Also list aliases
  * [mix local] Use ipv6 with auto fallback to ipv4 when downloading data
  * [mix profile] Allow all profiling tasks to run programatically
  * [mix test] Add `--failed` option that only runs previously failed tests
  * [mix test] Print coverage summary by default when the `--cover` flag is given
  * [Mix.Project] Add `Mix.Project.clear_deps_cache/0`
  * [Mix.Project] Add `Mix.Project.config_mtime/0` that caches the config mtime values to avoid filesystem access

### 2. Bug fixes

#### Elixir

  * [IO.ANSI.Docs] Fix table column alignment when converting docs to ANSI escapes
  * [Code] Ensure `string_to_quoted` returns error tuples instead of raising in certain constructs
  * [Code.Formatter] Consistently format keyword lists in function calls with and without parens
  * [Code.Formatter] Do not break after `->` when there are only comments and one-line clauses
  * [File] Allow the `:trim_bom` option to be used with `:encoding`
  * [Kernel] Raise on unsafe variables as some of the code emitted with unsafe variables would not correctly propagate variables or would disable tail call optimization semantics
  * [Kernel] Do not crash on dynamic sizes in binary generators with collectable into in comprehensions
  * [Kernel] Do not crash on literals with non-unary size in binary generators with collectable into in comprehensions
  * [Task] Improve error reports and exit reasons for failed tasks on Erlang/OTP 20+

#### ExUnit

  * [ExUnit.Case] Raise proper error if `@tag` and `@moduletag` are used before `use ExUnit.Case`
  * [ExUnit.Case] Raise proper error if `@describetag` is used outside of `describe/2` blocks
  * [ExUnit.DocTest] Emit proper assertion error on doctests with invalid UTF-8

#### Mix

  * [mix archive.install] Fetch optional dependencies when installing an archive from Git/Hex
  * [mix compile] Properly track config files in umbrella projects and recompile when any relevant umbrella configuration changes
  * [mix deps] Ensure the same dependency from different SCMs are tagged as diverged when those SCMs are remote and non-remote
  * [mix deps] Ensure we re-run dependency resolution when overriding a skipped dep in umbrella
  * [mix deps.compile] Perform clean builds for dependencies on outdated locks to avoid old modules from affecting future compilation
  * [mix escript.install] Fetch optional dependencies when installing an escript from Git/Hex

### 3. Soft-deprecations (no warnings emitted)

#### Elixir

  * [Code] Deprecate `Code.load_file/2` in favor of `Code.compile_file/2`
  * [Code] Deprecate `Code.loaded_files/0` in favor of `Code.required_files/0`
  * [Code] Deprecate `Code.unload_files/1` in favor of `Code.unrequire_files/1`

#### Logger

  * [Logger] `compile_time_purge_level` is deprecated in favor of `compile_time_purge_matching`

### 4. Hard-deprecations

#### Elixir

  * [Code] `Code.get_docs/2` is deprecated in favor of `Code.fetch_docs/1`
  * [Enum] `Enum.chunk/2/3/4` is deprecated in favor of `Enum.chunk_every/2/3/4` - notice `chunk_every` does not discard incomplete chunks by default
  * [GenServer] Warn if `super` is used in any of the GenServer callbacks
  * [Kernel] `not left in right` is ambiguous and is deprecated in favor of `left not in right`
  * [Kernel] Warn on confusing operator sequences, such as `1+++1` meaning `1 ++ +1` or `........` meaning `... .. ...`
  * [OptionParser] Deprecate dynamic option parser mode that depended on atoms to be previously loaded and therefore behaved inconsistently
  * [Stream] `Stream.chunk/2/3/4` is deprecated in favor of `Stream.chunk_every/2/3/4` - notice `chunk_every` does not discard incomplete chunks by default

## v1.6

The CHANGELOG for v1.6 releases can be found [in the v1.6 branch](https://github.com/elixir-lang/elixir/blob/v1.6/CHANGELOG.md).
