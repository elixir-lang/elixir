# Changelog for Elixir v1.8

## v1.8.0-dev

### 1. Enhancements

#### Elixir

  * [Calendar] Add `Calendar.TimeZoneDatabase` and a `Calendar.UTCOnlyTimeZoneDatabase` implementation
  * [DateTime] Add `DateTime.from_naive/3`, `DateTime.now/1` and `DateTime.shift_zone/3`
  * [File] Allow `:raw` option in `File.exists?/2`, `File.regular?/2` and `File.dir?/2`
  * [Inspect] Allow Inspect protocol to be derivable with the only/except options
  * [Kernel] Do not propagate counters to variables in quote inside another quote
  * [Kernel] Warn on ambiguous use of `::` and `|` in typespecs
  * [Kernel] Add `:delegate_to` `@doc` metadata tag when using `defdelegate`
  * [Kernel] Improve compile-time building of ranges via the `..` operator
  * [List] Add `List.myers_difference/3`
  * [Macro] Add `Macro.struct!/2` for proper struct resolution during compile time
  * [Map] Optimize and merge nested maps `put` and `merge` operations
  * [Range] Add `Range.disjoint?/2`
  * [Registry] Allow associating a value on `:via` tuple
  * [String] Add `String.bag_distance/2`

#### ExUnit

  * [ExUnit.Assertions] Show last N messages (instead of first N) from mailbox on `assert_receive` fail
  * [ExUnit] Add `ExUnit.after_suite/1` callback

#### IEx

  * [IEx.Server] Expose `IEx.Server` for custom IEx sessions

#### Mix

  * [Mix.Project] Add `:depth` and `:parents` options to `deps_paths/1`
  * [mix compile] Include optional dependencies in `:extra_applications`
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
  * [NaiveDateTime] Do not accept leap seconds in builder and parsing functions
  * [String] Fix ZWJ handling in Unicode grapheme clusters

#### Mix

  * [mix compile.protocols] Reconsolidate protocols if local deps is stale
  * [mix deps] Properly mark dependencies with different `:system_env` as diverged

### 3. Soft-deprecations (no warnings emitted)

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
