# Changelog for Elixir v1.8

## v1.8.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Optimize the default template engine to compile and execute more efficiently

#### Elixir

  * [Calendar] Add `Calendar.TimeZoneDatabase` and a `Calendar.UTCOnlyTimeZoneDatabase` implementation
  * [Calendar] Add callbacks `day_of_year/3`, `quarter_of_year/3`, `year_of_era/1`, and `day_of_era/3`
  * [Code.Formatter] Preserve user's choice of new line after most operators
  * [Date] Add `Date.day_of_year/1`, `Date.quarter_of_year/1`, `Date.year_of_era/1`, and `Date.day_of_era/1`
  * [DateTime] Add `DateTime.from_naive/3`, `DateTime.now/1`, and `DateTime.shift_zone/3`
  * [File] Allow `:raw` option in `File.exists?/2`, `File.regular?/2`, and `File.dir?/2`
  * [File] Allow POSIX time as an integer in `File.touch/2` and `File.touch!/2`
  * [Inspect] Allow `Inspect` protocol to be derivable with the `:only`/`:except` options
  * [Kernel] Do not propagate counters to variables in quote inside another quote
  * [Kernel] Warn on ambiguous use of `::` and `|` in typespecs
  * [Kernel] Add `:delegate_to` `@doc` metadata tag when using `defdelegate`
  * [Kernel] Improve compile-time building of ranges via the `..` operator
  * [Kernel] Compile charlist interpolation more efficiently
  * [Kernel.SpecialForms] Add `:reduce` option to `for` comprehensions
  * [List] Add `List.myers_difference/3` and `List.improper?/1`
  * [Macro] Add `Macro.struct!/2` for proper struct resolution during compile time
  * [Map] Optimize and merge nested maps `put` and `merge` operations
  * [Range] Add `Range.disjoint?/2`
  * [Record] Reduce memory allocation when updating multiple fields in a record
  * [Registry] Allow associating a value on `:via` tuple
  * [String] Add `String.bag_distance/2`
  * [Task] Add `$callers` tracking to `Task` - this makes it easier to find which process spawned a task and use it for tracking ownership and monitoring

#### ExUnit

  * [ExUnit] Add `ExUnit.after_suite/1` callback
  * [ExUnit.Assertions] Show last N messages (instead of first N) from mailbox on `assert_receive` fail

#### IEx

  * [IEx.Helpers] Add `port/1` and `port/2`
  * [IEx.Server] Expose `IEx.Server.run/1` for custom IEx sessions with the ability to broker pry sessions

#### Mix

  * [Mix] Add `Mix.target/0` and `Mix.target/1` to control dependency management per target
  * [Mix.Project] Add `:depth` and `:parents` options to `deps_paths/1`
  * [mix archive.install] Add a timeout when installing archives
  * [mix compile] Include optional dependencies in `:extra_applications`
  * [mix escript.install] Add a timeout when installing escripts
  * [mix format] Warn when the same file may be formatted by multiple `.formatter.exs`
  * [mix test] Allow setting the maximum number of failures via `--max-failures`
  * [mix test] Print a message instead of raising on unmatched tests inside umbrella projects

### 2. Bug fixes

#### Elixir

  * [Calendar] Allow printing dates with more than 9999 years
  * [Exception] Exclude deprecated functions in "did you mean?" hints
  * [Float] Handle subnormal floats in `Float.ratio/1`
  * [Kernel] Remove `Guard test tuple_size(...) can never succeed` dialyzer warning on try
  * [Kernel] Expand operands in `size*unit` bitstring modifier instead of expecting `size` and `unit` to be literal integers
  * [Kernel] Do not deadlock on circular struct dependencies in typespecs
  * [Kernel] Raise proper error message when passing flags to the Erlang compiler that Elixir cannot handle
  * [Kernel] Do not leak variables in `cond` clauses with a single matching at compile-time clause
  * [NaiveDateTime] Do not accept leap seconds in builder and parsing functions
  * [String] Fix ZWJ handling in Unicode grapheme clusters

#### IEx

  * [IEx.Helpers] Use typespec info (instead of docs chunk) and properly format callbacks in `b/1`

#### Logger

  * [Logger] Allow Logger backends to be dynamically removed when an application is shutting down

#### Mix

  * [mix compile] Ensure changes in deps propagate to all umbrella children - this fix a long standing issue where updating a dependency would not recompile all projecys accordingly, requiring a complete removal of `_build`
  * [mix compile] Avoid time drift when checking and updating compiler manifest files
  * [mix compile.app] Respect the `:only` option between umbrella siblings
  * [mix compile.protocols] Reconsolidate protocols if local dependencies are stale
  * [mix deps] Properly mark dependencies with different `:system_env` as diverged
  * [mix new] Use `--module` value when setting up filenames

### 3. Soft-deprecations (no warnings emitted)

None.

### 4. Hard-deprecations

#### Elixir

  * [Enum] Passing a non-empty list to `Enum.into/2` was inconsistent with maps and is deprecated in favor of `Kernel.++/2` or `Keyword.merge/2`
  * [Inspect.Algebra] `surround/3` is deprecated in favor of `Inspect.Algebra.concat/2` and `Inspect.Algebra.nest/2`
  * [Inspect.Algebra] `surround_many/6` is deprecated in favor of `container_doc/6`
  * [Kernel] Passing a non-empty list as `:into` in `for` comprehensions was inconsistent with maps and is deprecated in favor of `Kernel.++/2` or `Keyword.merge/2`
  * [Kernel.ParallelCompiler] `files/2` is deprecated in favor of `compile/2`
  * [Kernel.ParallelCompiler] `files_to_path/2` is deprecated in favor of `compile_to_path/2`
  * [Kernel.ParallelRequire] `files/2` is deprecated in favor of `Kernel.ParallelCompiler.require/2`
  * [System] `:seconds`, `:milliseconds`, etc. as time units is deprecated in favor of `:second`, `:millisecond`, etc.
  * [System] `System.cwd/0` and `System.cwd!/0` are deprecated in favor of `File.cwd/0` and `File.cwd!/0`

#### Mix

  * [mix compile.erlang] Returning `{:ok, contents}` or `:error` as the callback in `Mix.Compilers.Erlang.compile/6` is deprecated in favor of returning `{:ok, contents, warnings}` or `{:error, errors, warnings}`

## v1.7

The CHANGELOG for v1.7 releases can be found [in the v1.7 branch](https://github.com/elixir-lang/elixir/blob/v1.7/CHANGELOG.md).
