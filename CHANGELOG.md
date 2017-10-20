# Changelog for Elixir v1.6

## Compiler diagnostics

TODO.

## Code formatter

TODO.

## Stream data and property testing

TODO.

## v1.6.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Allow markers `/` and `|` to be used in a custom EEx engine

#### Elixir

  * [Code] Add `format_string!/2` and `format_file!/2` for automatic code formatting
  * [Inspect.Algebra] Add `:strict` and `:flex` breaks
  * [Inspect.Algebra] Allow a group to inherit the parent group break
  * [Inspect.Algebra] Add `force_break/1` and `next_break_fits/2` which give more control over document fitting
  * [Inspect.Algebra] Add `collapse_lines/1` for collapsing multiple lines to a maximum value
  * [Inspect.Algebra] Allow `nest/2` to be `:reset` or be set to the current `:cursor` position
  * [Kernel] Prefix variables with V when emitting Erlang code. This improves the integration with tools such as Erlang code formatters and the GUI debugger
  * [Kernel] Warn on the use of `length(x) == 0` in guards
  * [Kernel] Warn if `catch` comes before `rescue` in try
  * [Kernel.ParallelCompiler] Add `compile/2`, `compile_to_path/3` and `require/2` which provide detailed information about warnings and errors
  * [Stream] Add `Stream.intersperse/2`
  * [String] Update to Unicode 10
  * [String] Allow passing empty string `match` to `String.replace/4`
  * [Task] Allow a custom supervisor to be given to `Task.Supervisor.async_stream/3`
  * [Time] Add `Time.add/3`

#### ExUnit

  * [ExUnit.Callbacks] Add `ExUnit.Callbacks.start_supervised!/2`
  * [ExUnit.Case] Generate a random seed per test based on the test suite seed

#### Mix

  * [mix archive.build] Allow `mix archive.build` to bundle dot files via an option
  * [mix compile] Define a behavior for Mix compiler tasks and return diagnostics from compiler tasks
  * [mix deps] Support `:system_env` option when specifying dependencies
  * [mix format] Add a `mix format` task that formats the given files (or the files specified in a `.formatter.exs` file)
  * [mix profile.eprof] Add a new task for time-based profiling with eprof
  * [mix test] Run all functions in a describe block by giving the `file:line` the describe block starts
  * [mix test] Report the top N slowest tests with the `--slowest N` flag

### 2. Bug fixes

#### Elixir

  * [CLI] Support path with spaces as argument to elixir.bat
  * [Kernel] Solve a precedence issue between `&` and `|`, such as `[&Foo.bar/1 | &Baz.bat/2]`
  * [Kernel] Do not load dynamic Elixir modules as `:in_memory` as this value is not officially supported by the code server. Instead, use an empty list, which is the same value used by Erlang.
  * [Kernel] Validate variable struct name is atom when used in pattern matching
  * [Macro] Fix `Macro.to_string/2` for tuple calls, such as `alias Foo.{Bar, Baz}`
  * [MapSet] Return valid MapSet when unioning a legacy MapSet
  * [URI] Preserve empty fragments in `URI.parse/1`

#### Mix

  * [mix deps] Ensure optional dependencies in umbrella applications are loaded
  * [mix xref] Take compile dependencies with higher priority that runtime ones when building a graph

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Inspect.Algebra] `surround/3` and `surround_many/6` are deprecated in favor of `container_doc/6`
  * [Kernel.ParallelCompiler] `files/2` and `files_to_path/3` are deprecated in favor of `compile/2` and `compile_to_path/3`
  * [Kernel.ParallelRequire] `files/2` is deprecated in favor of `Kernel.ParallelCompiler.require/2`

#### ExUnit

  * [ExUnit.Formatter] `:case_started` and `:case_finished` events are deprecated in favor of `:module_started` and `:module_finished`

### 4. Deprecations

#### Elixir

  * [Enum] `Enum.partition/2` is deprecated in favor of `Enum.split_with/2`
  * [Keyword] `Keyword.replace/3` is deprecated in favor of `Keyword.fetch/2` and `Keyword.put/3`
  * [Map] `Map.replace/3` is deprecated in favor of `Map.fetch/2` and `Map.put/3`
  * [Range] Deprecate `Range.range?/1` in favor of pattern matching on `_ .. _`

## v1.5

The CHANGELOG for v1.5 releases can be found [in the v1.5 branch](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md).
