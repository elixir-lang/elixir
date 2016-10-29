# Changelog for Elixir v1.2

v1.2 brings enhancements, bug fixes, performance improvements and more
into Elixir. Elixir v1.2 relies on many features in Erlang 18, requiring
at least Erlang 18+. Upgrading to Erlang 18 is therefore necessary before
upgrading Elixir.

## Erlang 18 support

We have brought many features specific to Erlang 18. Here are the highlights:

  * Maps can now scale from dozens to millions of keys. Therefore, usage of
    the modules `Dict` and `HashDict` is now discouraged and will be
    deprecated in future releases, instead use `Map`. Similarly, `Set` and
    `HashSet` will be deprecated in favor of `MapSet`
  * Compilation times are faster due to improvements in both the Elixir and
    Erlang compilers
  * Dialyzer now emits less false negative warnings thanks to new annotations
    available in the Erlang compiler

## Language improvements

This release includes four notable language improvements:

  * The addition of multi aliases/imports/require:

        alias MyApp.{Foo, Bar, Baz}

  * Support for variables in map keys:

        %{key => value}

  * Support for the pin operator in map keys and function clauses:

        %{^key => value} = %{key => value}
        fn ^key -> :ok end

  * Addition of the `with` special form to match on multiple expressions:

        with {:ok, contents} <- File.read("my_file.ex"),
             {res, binding} <- Code.eval_string(contents),
             do: {:ok, res}

These improvements aim to make the language more consistent and expressive.

## Getting started experience

While we were improving the language, we also improved both the parser and
compiler to be even more aware of language constructs, emitting warnings
on common pitfalls like when piping to expressions without parentheses or
when defining unsafe variables.

We have also introduced the `i/1` helper in IEx, which allows developers
to retrieve information about any data type. This will help newcomers
explore the language values while providing experienced developers with
crucial information about the value they are introspecting.

## Workflow improvements

Umbrella applications are now able to share both build and configuration files.
This aims to drastically reduce compilation times in umbrella projects by
adding the following configuration to each umbrella app's `mix.exs` file:

    build_path: "../../_build",
    config_path: "../../config/config.exs",

Finally, Mix will now consolidate protocols by default as we are now able to
consolidate in parallel and cache the consolidation results, providing the
best performance across all environments without affecting compilation times.
The only downside of this change is that, if you have been implementing
protocols exclusively as part of your test suite, inside the `test` directory,
those won't be picked up as it happens after compilation. For such cases,
consolidation can be disabled by setting `consolidate_protocols: false` in
the project config.

These are great additions on top of the faster compilation times we have
achieved when migrating to Erlang 18.

## Rebar 3 support

With Rebar 3 gaining more adoption in the Erlang community, Mix is
now able to fetch and compile Rebar 3 dependencies. This feature is currently
experimental and therefore opt-in: if you have a Rebar 3 dependency, you can
ask Mix to use Rebar 3 to compile it by passing the `manager: :rebar3` option.
Once configured, Mix will prompt you to install Rebar 3 if it is not yet
available.

## v1.2.6 (2016-06-06)

### 1. Enhancements

  * [Kernel] Support Erlang 19
  * [Kernel] Supported generated: true in the `quote` special form

### 2. Bug fixes

  * [Path] Fix a bug in path join with "/" followed by empty segments
  * [String] Fix a bug in NFD normalization when followed by one-byte sized graphemes
  * [Typespec] Correctly support `<<_::size, _::_*unit>>` syntax

## v1.2.5 (2016-04-30)

### 1. Bug fixes

  * [Logger] Stringify truncated function data in Logger
  * [Logger] Ensure poorly formatted char data can also be logged by using the replacement character "�" (diamond question mark)
  * [Mix] Do not assume `@impl` is always a list
  * [String] Fix bugs in `String.replace_*` functions where it would not include the accumulated value for certain replacements

## v1.2.4 (2016-04-01)

### 1. Enhancements

  * [Mix] Add `:archives` configuration to `def project` that allows projects to list archive dependencies. `--no-archives-check` (as well as `--no-deps-check`) will disable the archive check. The `:archives` option is not checked for dependencies.
  * [Mix] Add `deps.precompile` task as hook
  * [Mix] Support `--include-children` in `mix deps.compile` option
  * [String] Update version of the Unicode database to 8.0.0

### 2. Bug fixes

  * [Application] Ensure `spec/2` returns nil for unknown applications
  * [Integer] Fix a possible binary leak in `parse/1`
  * [Mix] Purge Erlang modules on recompilation
  * [String] Ensure `split/1` does not break on non-breakable whitespace
  * [String] Ensure NFC and NFD normalization pass all of Unicode 8.0.0 tests
  * [Version] Allow dots in build info for versions in `Version.parse/1`

## v1.2.3 (2016-02-21)

### 1. Enhancements

  * [Base] Add `:ignore` and `:padding` option to encoding/decoding functions
  * [Mix] Add `Mix.Projects.deps_paths` that returns the dependencies path as a map

### 2. Bug fixes

  * [ExUnit] Do not provide negative line numbers without generated annotation (for compatibility with Erlang 19)
  * [Mix] Reject non fullfilled optional dependencies later on in the convergence resolution for proper dependency sorting
  * [String] Fix incomplete data trimming on both `String.replace_trailing` and `String.rstrip`
  * [String] Attach debug_info back into Unicode modules for Dialyzer support

## v1.2.2 (2016-01-31)

### 1. Enhancements

  * [Kernel] Support `@compile {:autoload, false}` to disable automatic loading after compilation

### 2. Bug fixes

  * [ExUnit] Raise if trying to override reserved tag in `setup` blocks
  * [Mix] Ensure retrieve compile manifests do fail if some compilers are not yet available
  * [Mix] Automatically merge managers according to the mix > rebar3 > rebar > make order
  * [Mix] Force recompilation if dependency was recently fetched

## v1.2.1 (2016-01-14)

### 1. Enhancements

  * [IEx] Support remote pids/ports with IEx helper `i/1`
  * [Protocol] Warn when `defimpl` is called for a consolidated protocol

### 2. Bug fixes

  * [ExUnit] Ensure `assert` macros can be used from quoted code
  * [ExUnit] Do not warn in match assertion if variable is reused in pattern
  * [Macro] Fix a bug in `Macro.to_string/1` where a remote function could be accidentally interpreted as a sigil
  * [Mix] Ensure dependencies are properly skipped when `--only` option is given to `mix deps.get`

## v1.2.0 (2016-01-01)

### 1. Enhancements

#### Elixir

  * [Application] Add `spec/1` and `spec/2` to retrieve application specification
  * [Application] Add `get_application/1` to retrieve the application a given module belongs to
  * [Base] Optimize encode and decode operations about 10 times
  * [Enum] Use the faster and auto-seeding `:rand` instead of `:random` in `Enum.shuffle/1` and `Enum.random/1` and `Enum.take_random/2`
  * [Enum] Add `Enum.with_index/2`
  * [GenServer] Add `GenServer.stop/1` for shutting down servers reliably
  * [IO] Add `color` related functions to `IO.ANSI`
  * [Kernel] Support multiple aliases in `alias`, `import`, `require` and `use`. For example, `alias MyApp.{Foo, Bar, Baz}`
  * [Kernel] Add `struct!/2`. Similar to `struct/2` but raises on invalid keys
  * [Kernel] Warn if `@doc/@typedoc/@moduledoc` attributes are redefined
  * [Kernel] Warn if non-variables are used in `defdelegate/2` (as they have no effect)
  * [Kernel] Mark quoted expressions as generated, avoiding false positives on dialyzer
  * [Kernel] Allow variables as map keys on creation `%{key => value}` and on matches `%{^key => value}`
  * [Kernel] Allow the pin operator `^` in `fn` clauses and on the left side of `<-` in `for` comprehensions
  * [Kernel] Introduce `with` as a special form that allows matching on right side parameters
  * [Kernel] Warn when right hand side of `->` does not provide any expression
  * [Kernel] Warn if the Elixir was compiled with a different endianness than the one currently available at runtime
  * [Kernel] Warn if a variable is used after being defined exclusively in a nested context
  * [Kernel] Warn if piping into an expression without parentheses
  * [Macro] Add `Macro.traverse/4` that performs pre and post-walk at once
  * [Macro] Add `Macro.camelize/1` and `Macro.underscore/1`
  * [Process] Add `Process.get_keys/0`
  * [Stream] Add `Stream.with_index/2`
  * [String] Introduce `String.replace_{prefix,suffix,leading,trailing}/2`. The first two will replace only the first occurrence of the given match in string. The last two will replace all occurrences of the given match
  * [String] Support `String.normalize/2` and `String.equivalent?/2` that perform NFD and NFC normalization
  * [System] Add `System.time_offset`, `System.monotonic_time`, `System.system_time`, `System.convert_time_unit` and `System.unique_integer`
  * [System] Allow `System.cmd/3` to remove variables by specifying nil values
  * [Task] Add `Task.Supervisor.async_nolink/1/3` that spawns a supervised task without linking to the caller process
  * [Task] Introduce `Task.yield_many/2`
  * [Task] Raise an error when a task is queried from a non-owning process (instead of waiting forever)

#### ExUnit

  * [ExUnit] Allow one test to raise multiple errors. The goal is to enable tools in the ecosystem to emit multiple failure reports from the same test
  * [ExUnit] Support `@tag report: [:foo, :bar]` which will include the values for tags `:foo` and `:bar` whenever a test fails

#### IEx

  * [IEx] Allow `IEX_WITH_WERL` to be set on Windows to always run on WERL mode
  * [IEx] Display type docs for `t(Module.type)` and `t(Module.type/arity)`
  * [IEx] Add `i/1` helper that prints information about any data type
  * [IEx] Show source code snippet whenever there is a request to pry a given process

#### Logger

  * [Logger] Add file to logger metadata

#### Mix

  * [Mix] Cache and always consolidate protocols
  * [Mix] Add `warn_test_pattern` to `mix test` that will warn on potentially misconfigured test files
  * [Mix] Introduce `MIX_QUIET` environment variable that configures the underlying Mix task to output only error messages
  * [Mix] Introduce `MIX_DEBUG` environment variable that prints information about the task being run
  * [Mix] Validate git options and warn on conflicting ref, branch or tags
  * [Mix] New umbrella applications will now share configuration and build files
  * [Mix] Add experimental support for Rebar 3
  * [Mix] Do not warn when an optional dependency has a conflicting `:only` option with another dependency
  * [Mix] Raise readable error message when parsertools is not available
  * [Mix] Add `--build` flag to `mix deps.clean DEP` to only remove artifacts from `_build`

### 2. Bug fixes

#### Kernel

  * [Access] Improve error messages when using Access on non-valid key-value structures
  * [Kernel] Raise when conflicting `:only` and `:except` are given to import
  * [Kernel] Change `__ENV__.file` if `@file` is set for the given function
  * [Kernel] Make `Kernel.ParallelRequire` aware of `:warning_as_errors`
  * [Kernel] Improve error message for invalid `do`/`do:`
  * [Macro] Ensure `Macro.to_string/2` respects operator precedence when using the access operator
  * [Path] Do not crash when expanding paths that go beyond the root, for example, `Path.expand("/../..")`
  * [String] Ensure `UnicodeConversionError` does not contain invalid string in its error message

#### IEx

  * [IEx] Do not start apps on `recompile` helper if `--no-start` was given
  * [IEx] Avoid copying of data when evaluating every expression in IEx

#### Mix

  * [Mix] Always run non-recursive tasks at the umbrella root
  * [Mix] Ensure rebar projects work on directory names that contain non-latin characters
  * [Mix] Ignore directories inside `apps` in umbrellas that do not have a `mix.exs` file
  * [Mix] Ensure Mix can be used with path dependencies where the app name is different than the path basename
  * [Mix] Ensure dependencies won't crash when updating from a git repository to a hex repository and the git version did not respect SemVer
  * [Mix] Do not run remote converger if dependencies have diverged
  * [Mix] Ensure umbrella dependencies across all environments are loaded on parent deps.get/deps.update

#### ExUnit

  * [ExUnit] Include file and line in all compilation errors for doctests

### 3. Soft deprecations (no warnings emitted)

#### Kernel

  * [Dict] `Dict` and `HashDict` are soft deprecated in favor of `Map`
  * [Keyword] `Keyword.size/1` is deprecated in favor of `length/1`
  * [Map] `Map.size/1` is deprecated in favor of `map_size/1`
  * [Set] `Set` and `HashSet` are soft deprecated in favor of `MapSet`

#### Mix

  * [Mix] `Mix.Utils.camelize/1` and `Mix.Utils.underscore/1` are soft deprecated in favor of `Macro.camelize/1` and `Macro.underscore/1`

## v1.1

The CHANGELOG for v1.1 releases can be found [in the v1.1 branch](https://github.com/elixir-lang/elixir/blob/v1.1/CHANGELOG.md).
