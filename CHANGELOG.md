<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.21

## v1.21.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Support splitting middle expressions across EEx clauses

#### Elixir

  * [Access] Add support for keyword lists in `Access.key/2` and `Access.key!/1`
  * [Code] Add `:purge` and `:purge_all` compilation tracer events
  * [Code] Add `:max_integer_bytes` option to `Code.string_to_quoted/2` and friends to limit the size of integer literals
  * [Code] Add support for the `:erlc_options` compiler option
  * [Code.Formatter] Add a `:migrate_atom_interpolations` option
  * [Code.Formatter] Preserve comments at their intended location in many different scenarios
  * [File] Accept an open IO device in `File.stat/2`, `File.stat!/2`, `File.regular?/2`, `File.dir?/2`, and `File.exists?/2`
  * [IO.ANSI] Add `bold/0`, `bold_off/0`, `dim/0`, `dim_off/0`, `blink/0`, `invisible/0`, `invisible_off/0`, `strikethrough/0`, `strikethrough_off/0`, `italic_off/0`, `underline_off/0`, `overline/0`, `overline_off/0`, `cursor_forward/0`, `cursor_backward/0`, and `cursor_home/0`, aligning names with Erlang/OTP
  * [IO.ANSI.Docs] Recognize additional punctuation delimiters when rendering Markdown
  * [IO.ANSI.Docs] Strip inline attribute lists, such as `{: .warning}`, from headings, including inside blockquotes
  * [Kernel] Improve performance of type constructors and complex intersections
  * [Kernel] Refine variables across `or`, `||`, and the falsy side of `and` and `&&` in conditions, such as `cond do is_integer(x) or is_boolean(x) -> ... end`
  * [Kernel] Refine variables through the assignments in conditionals, such as `y = is_integer(x); if y, do: x, else: x`
  * [Kernel] Warn on aliases that are never used because a later alias shadows them
  * [Kernel] Warn on binary patterns with segments that are not byte-aligned
  * [Kernel] Improve type precision of `--/2`
  * [Kernel] Improve type precision of `case` when some of its clauses always raise
  * [Kernel] Improve type precision of `case` and `if` on literals by ignoring unreachable clauses
  * [Kernel] Make pin patterns with singleton types precise, so subsequent redundant clauses are detected
  * [Kernel.ParallelCompiler] Add a hint when spawned processes cannot load modules defined during compilation
  * [Keyword] Optimize `Keyword.pop/3`, `Keyword.pop!/2`, and `Keyword.pop_lazy/3`
  * [List] Add `List.to_existing_atom/2` and `List.to_unsafe_atom/1`
  * [Macro] Accept a callback in `Macro.quoted_literal?/2` to consider additional nodes, such as variables, as literals
  * [MapSet] Optimize `MapSet.symmetric_difference/2` when set sizes differ
  * [Path] Add `Path.safe_join/2`
  * [Protocol] Add the `:skip_consolidated` option to `Protocol.extract_protocols/2`
  * [Protocol] Warn when implementing a protocol for a module that does not define a struct
  * [Registry] Add the `:spawn_opt` option to `Registry.start_link/1`
  * [Registry] Optimize exact key matching in lookups
  * [String] Optimize `String.bag_distance/2`
  * [String] Add `String.to_existing_atom/2` and `String.to_unsafe_atom/1`
  * [String] Update to Unicode 18.0.0
  * [URI] Optimize percent-decoding and `URI.to_string/1`

#### ExUnit

  * [ExUnit.Assertions] Add `trace/3` helper
  * [ExUnit.CLIFormatter] Improve rendering of test locations in some terminals

#### Mix

  * [mix deps.compile] Start long dependency chains first when compiling dependencies across OS partitions
  * [mix test] Add `--coverage-format` to emit coverage reports as `html` (the default) or `text`
  * [mix xref] List the compile-connected files with most transitive dependencies in `mix xref graph --format stats`
  * [mix xref] Add `mix xref explain FILE` to list the compile-time dependents and the dependencies of a file

### 2. Bug fixes

#### Potential breaking changes

The following bug fixes may break applications relying on the broken behaviour:

  * [Kernel] Fix parsing precedence of unary operators applied to do-end blocks, such as `!if a do b end || c`
  * [Kernel] Parse chained `in` and `not in` operators as left associative
  * [Kernel] Raise on empty exception lists in `rescue`, such as `rescue e in []`, instead of rescuing every error
  * [Process] Use `exit_signal/2` from `:erlang` in `Process.exit/2`, so exit signals sent to the calling process are delivered asynchronously and `:normal` no longer terminates it
  * [ExUnit.Assertions] Do not leak variables bound on one side of `assert left op right` into the other side, which could make false assertions pass

#### Elixir

  * [bin/elixirc] Fail compilation on warnings when `--warnings-as-errors` is given
  * [Calendar] Fix `Calendar.strftime/3` formatting of negative years with `%y`
  * [Calendar] Fix rounding for `:day`, `:hour`, and `:minute` units in `DateTime.diff/3`, `NaiveDateTime.diff/3`, and `Time.diff/3`
  * [Calendar.ISO] Fix `Calendar.ISO.valid_time?/4` to reject non-integer microsecond precision
  * [Calendar.ISO] Reject negative zero UTC offsets in basic formats
  * [Code] Fall back to `doc/chunks` in `Code.fetch_docs/1` when given the path to a `.beam` file without documentation
  * [Code] Properly escape C1 control characters and Unicode noncharacters in `Code.quoted_to_algebra/2` (and `Macro.to_string/1`)
  * [Code] Preserve opening and closing metadata of parenthesized unary operators, such as `(!x)` and `(not x)`, when `:token_metadata` is enabled
  * [Code.Formatter] Fix rendering calls where `do` is followed by non-block keyword arguments
  * [Code.Formatter] Keep parentheses on anonymous function calls with do-end blocks
  * [Code.Formatter] Keep parentheses on unary operators on the left side of `in`, such as `(!foo) in bar`
  * [Code.Fragment] Fix cursor completion when operator keywords such as `in`, `when`, `and`, `or`, and `not` follow another operator
  * [Code.Fragment] Respect the `:file`, `:line`, and `:column` options in `Code.Fragment.container_cursor_to_quoted/2`
  * [Date] Preserve the `:format` option in `Date.to_iso8601/2` with custom calendars
  * [Date.Range] Fix slicing date ranges with stepped ranges returning the wrong results
  * [DateTime] Keep the calendar of the given map in `DateTime.convert/2` and `DateTime.truncate/2`
  * [DateTime] Truncate fractional differences towards zero in `DateTime.diff/3`
  * [Duration] Reject duplicate seconds in `Duration.from_iso8601/1`
  * [DynamicSupervisor] Do not discard the exit reason of children that terminate during shutdown
  * [Enum] Fix `Enum.min/2,3` and `Enum.max/2,3` with custom sorters on ranges
  * [Enum] Match `Enum.slice/2` semantics in `Enum.slide/3` for negative insertion indices and ranges out of bounds
  * [File] Return `{:error, :badarg, path}` from `File.cp_r/3` and `File.rm_rf/1` on paths with null bytes instead of raising, aligning with Erlang/OTP
  * [File] Trim UTF-32 LE BOMs in `File.stream!/3`
  * [IO.ANSI.Docs] Do not crash on unclosed HTML comments
  * [Inspect.Algebra] Break optimistic groups when their parent group breaks
  * [Kernel] Check range endpoints are integers in guards using `in` with literal steps, such as `x in 0..max//1`
  * [Kernel] Continue compilation after reporting repeated map keys in patterns
  * [Kernel] Do not consider local functions invoked in bitstring size expressions inside macros as unused
  * [Kernel] Do not expand macros twice in `in/2`, in multi-alias options, such as `alias Foo.{Bar, Baz}, warn: false`, and in `@before_compile`, `@after_compile`, and `@derive` callbacks
  * [Kernel] Do not define aliases in nested `defmodule` calls whose name starts with a fully qualified module, such as `defmodule unquote(__MODULE__).Child`
  * [Kernel] Do not dispatch to protocols when folding calls at compile time, such as `MapSet.new/1` and `Duration.new!/1` with non-list arguments
  * [Kernel] Do not evaluate generator guards in `for` comprehensions as body expressions, which raised instead of filtering when they failed
  * [Kernel] Do not fold `URI.parse/1`, `URI.new/1`, and `URI.new!/1` at compile time, as they depend on the default ports configured at runtime
  * [Kernel] Do not warn on comparisons between data structures with numbers that may be equal, such as `{1} == {1.0}`, nor discard such values when narrowing types
  * [Kernel] Expand `defguard` macros separately in guard and body contexts, preserving `and`/`or` error semantics outside guards
  * [Kernel] Fix `unquote` in `quote` options, such as `quote bind_quoted: [x: unquote(v)]`, inside unquote fragments
  * [Kernel] Raise a clear error message when using `__CALLER__` and `__STACKTRACE__` within guards of the constructs that introduce them
  * [Kernel] Fix compilation of maps whose keys contain patterns or map updates, such as `%{case x do %{a: v} -> v end => :value}`
  * [Kernel] Fix inferred stacktrace types to allow arbitrary keyword metadata
  * [Kernel] Fix false positive type warnings on guards which may fail depending on values, such as `div/2` and `map_get/2` with non-atom keys, especially as operands of `and`/`or`
  * [Kernel] Fix printing of list types in type warnings, which could show wider types or misplace exclusions
  * [Kernel] Fix type information about variables defined in one default argument leaking into subsequent default arguments
  * [Kernel] Follow UAX #9 levels when computing the bidi skeleton of confusable identifiers
  * [Kernel] Raise when `|` is used in guards
  * [Kernel] Raise on duplicate keys in `@enforce_keys`
  * [Kernel] Raise on anonymous function calls on atoms, such as `:foo.()`, instead of invoking the local function
  * [Kernel] Raise on guards that reference themselves or guards without a body, instead of hanging compilation
  * [Kernel] Raise a clear error on explicit tails in `destructure/2`, such as `destructure([a | b], list)`
  * [Kernel] Do not let aliases defined in macro arguments affect the aliases in the macro's quoted code
  * [Kernel] Do not accept empty tuples in `elem/2` and other tuple operations during type checking
  * [Kernel] Fix inferred types of comprehensions using `:into` with binaries and bitstrings
  * [Kernel] Fix type checker crash on functions with impossible clauses
  * [Kernel] Fix type checker crash on `defimpl` for modules that are not Elixir structs
  * [Kernel] Fix type checker crash on variables used by nested clauses in conditionals
  * [Kernel] Fix type checker crash on `for` generators using variables bound in `:into` or `:reduce`
  * [Kernel] Fix type checker crash on patterns that cannot match in `fn` and `catch` clauses with guards
  * [Kernel] Fix type checker crash and imprecise types on tuple operations over types holding empty tuples
  * [Kernel] Keep atom keys in the inferred type of `Map.from_keys/2` when given mixed keys
  * [Kernel] Preserve alignment of untyped nested bitstring segments, such as `<<<<x::1>>::binary>>`
  * [Kernel] Preserve evaluation order when the compiler merges map updates with `Map.merge/2`
  * [Kernel] Report applying empty function types as bad functions instead of bad arities
  * [Kernel] Return `dynamic()` from `min/2` and `max/2` when their arguments are dynamic
  * [Kernel] Skip non-matching chunks in bitstring generators instead of halting the comprehension in some cases, such as with `:uniq`
  * [Kernel] Support Erlang modules in `import Mod, only: :sigils`
  * [Kernel] Warn on modules defined more than once in the same compilation (regression from previous Elixir versions)
  * [Kernel.CLI] Keep the exit status when the error cannot be printed to stderr
  * [Kernel.ParallelCompiler] Do not leak processes when compilation fails
  * [Kernel.ParallelCompiler] Do not return `{:module, module}` from `Code.ensure_compiled/1` for modules that do not exist after a deadlock is released
  * [Kernel.ParallelCompiler] Load newly compiled modules when a previous version of the module is already loaded
  * [Keyword] Delete duplicate keys when `Keyword.get_and_update/3` and `Keyword.get_and_update!/3` return `:pop`
  * [Macro] Respect the disabled ANSI setting in `Macro.dbg/3`
  * [Macro] Return `true` from `Macro.quoted_literal?/1` for literals wrapped in `__block__`
  * [Macro] Return valid AST from `Macro.pipe/3` when piping into `Access.get/3`
  * [NaiveDateTime] Fix `NaiveDateTime.diff/3` over-counting incomplete units
  * [OptionParser] Report missing and invalid arguments for switches with `:keep`
  * [Path] Return `[]` from `Path.wildcard/2` on paths with null bytes instead of raising, aligning with Erlang/OTP
  * [Range] Fix `Range.disjoint?/2` for ranges beyond floating-point precision
  * [Range] Fix `Range.disjoint?/2` for single-element ranges with a negative step
  * [Record] Evaluate the arguments of record macros from left to right and only once, including the record being updated
  * [Regex] Escape U+0085, U+200E, U+200F, U+2028, and U+2029 in `Regex.escape/1`, as Erlang/OTP 28+ ignores them in extended mode
  * [Stream] Close resources in `Stream.zip/1,2`, `Stream.zip_with/2,3`, and `Stream.cycle/1` when the consumer or one of the streams raises
  * [String] Fix `String.reverse/1` grapheme ordering around invalid UTF-8 bytes
  * [String] Return `1.0` from `String.bag_distance/2` for two empty strings
  * [System] Do not drop the final unterminated line in `System.cmd/3` with `:lines`
  * [System] Return `nil` from `System.find_executable/1` on programs with null bytes instead of raising, aligning with Erlang/OTP
  * [Task] Do not leave replies in the caller's mailbox when `Task.ignore/1`, `Task.await/2`, and `Task.await_many/2` exit
  * [Task] Report the actual exit reason, instead of `:noproc`, of tasks in `Task.async_stream/3` and `Task.Supervisor.async_stream/4` which exit immediately
  * [Task] Return the exit reason of tasks, instead of `{:exit, :noproc}`, from `Task.shutdown/2` when they terminate before being monitored
  * [Time] Validate microseconds in `Time.from_seconds_after_midnight/3`

#### ExUnit

  * [ExUnit] Warn on invalid line filters instead of raising in `ExUnit.Filters.parse/1`
  * [ExUnit] Do not count excluded tests towards `--max-failures` on `setup_all` failures
  * [ExUnit] Give each parameter set of parameterized tests its own random seed
  * [ExUnit] List tests in execution order with `--dry-run`
  * [ExUnit] Preserve the order of async modules across `--repeat-until-failure` runs
  * [ExUnit] Record tests invalidated by module failures as failures in the failures manifest
  * [ExUnit.Assertions] Do not expand the arguments of `match?/2` twice in `assert/1` and `refute/1`
  * [ExUnit.Assertions] Fix `refute_in_delta/4` at the delta boundary and with negative deltas
  * [ExUnit.Assertions] Support list literals in `catch_throw/1`, `catch_exit/1`, and `catch_error/1`
  * [ExUnit.CaptureIO] Close the capture device when `capture_io` fails to set the group leader
  * [ExUnit.CLIFormatter] Color the summary yellow only when all tests are excluded
  * [ExUnit.CLIFormatter] Report failures whose messages contain invalid UTF-8
  * [ExUnit.DocTest] Fix line numbers in `doctest_file/2` failures
  * [ExUnit.DocTest] Fix the expected line in doctests with multi-line expressions

#### IEx

  * [IEx.Autocomplete] Fix completion crashes on maps with non-atom keys
  * [IEx.Autocomplete] Do not split Unicode codepoints when completing map keys
  * [IEx.Evaluator] Recognize `**` and `not in` as continuation operators
  * [IEx.Helpers] Do not crash `process_info/1` on processes monitoring others by name
  * [IEx.Helpers] Fix `r/1` when multiple modules are defined in the same file
  * [IEx.Helpers] Fix heap and stack memory calculations in `process_info/1`
  * [IEx.Helpers] Fix showing macro specs in `h/1`
  * [IEx.Helpers] Shell-quote file paths given to the editor in `open/1`

#### Logger

  * [Logger] Restore the Erlang default handler when the `:logger` application stops
  * [Logger.Backends] Do not remove the backends handler when formatting an event crashes, such as on a raising `:report_cb`
  * [Logger.Backends] Support all handler configuration changes, such as `:logger.set_handler_config/3`, in the backends handler
  * [Logger.Formatter] Prune surrogate code points in `Logger.Formatter.prune/1`

#### Mix

  * [Mix] Ensure the lock and pubsub directories in the system tmp directory are owned by the current user
  * [Mix.Generator] Support iodata contents in `create_file/3` with `:format_elixir`
  * [mix cmd] Exit with the status of the failing command
  * [mix compile] Compile optional dependencies that non-optional dependencies require when `--no-optional-deps` is given
  * [mix compile] Support `--list` when `compile` is aliased
  * [mix compile.elixir] Emit absolute `:source` paths in diagnostics from `@after_verify` callbacks, so Mix persists them in its manifests
  * [mix compile.elixir] Recompile the project when `--docs` or `--debug-info` change
  * [mix compile.elixir] Purge consolidated protocols on recompilation even when the consolidation path was already removed from the code path
  * [mix compile.elixir] Recompile modules affected by export changes of their transitive dependencies, such as struct field changes
  * [mix compile.elixir] Recompile compile-time dependents of modules that are recompiled in a later cycle of the same compilation
  * [mix compile.elixir] Recompile files using, at compile time, modules defined in the same file when the dependencies of those modules change
  * [mix compile.elixir] Recompile dependents of Erlang modules from path dependencies when the Erlang modules change
  * [mix compile.elixir] Recompile the project when configuration files are removed
  * [mix compile.elixir] Verify runtime dependents of removed modules, so calls to them emit warnings
  * [mix compile.elixir] Verify callers of protocols when implementations are added or removed
  * [mix deps] Recompile path and fetchable dependencies when one of their dependencies are removed
  * [mix deps] Raise on dependency names containing `.`, `/`, `\`, or `:` to avoid path traversal
  * [mix deps] Handle empty refs and charlist sparse directories in rebar Git dependencies
  * [mix deps] Raise if Git refspecs start with `-`
  * [mix deps] Mark fetchable dependencies for compilation when their build exists but their SCM manifest is missing
  * [mix deps] Preserve code paths and compiler options across OS partitions
  * [mix deps.compile] Do not load dependencies inside a dependency when one of its tasks, such as a compiler, cannot be found, and suggest `--include-children` instead
  * [mix escript.install] Respect the `:path` escript option and raise on URLs without a path
  * [mix format] Invalidate the cached configuration when imported `.formatter.exs` files change
  * [mix format] Pass `:sigils` to plugins, allowing nested sigils to be formatted
  * [mix local.hex] Install Hex with `--if-missing` when the installed version is older than the requested one
  * [mix new] Avoid trailing whitespace in generated files
  * [mix profile.tprof] Forward script arguments
  * [mix release] Keep `:erts` in the release applications
  * [mix test] Do not crash the coverage summary when there are no modules
  * [mix test] Do not let the coverage threshold override the exit status of test failures
  * [mix test] Consider removed modules, local dependencies, protocol implementations, and Erlang modules in `--stale`
  * [mix xref] Include the sink in `mix xref graph --format stats --sink FILE` when it has no dependencies of its own

### 3. Hard deprecations

#### Elixir

  * [Macro.Env] `Macro.Env.fetch_alias/2` and `Macro.Env.fetch_macro_alias/2` are deprecated, use `Macro.Env.expand_alias/4` instead
  * [Path] `Path.safe_relative_to/2` is deprecated, use `Path.safe_relative/2` instead

#### Mix

  * [mix compile.elixir] `--tracer` is deprecated, set `:tracers` in `:elixirc_options` or use `Code.put_compiler_option/2` instead

### 4. Soft deprecations

#### Elixir

  * [IO.ANSI] `blink_slow/0`, `conceal/0`, `crossed_out/0`, `cursor_left/0`, `cursor_right/0`, `home/0`, `no_underline/0`, `not_crossed_out/0`, `not_italic/0`, `not_overlined/0`, `overlined/0`, `reveal/0`, `reverse/0`, and `reverse_off/0` are deprecated in favor of the names aligned with Erlang/OTP
  * [Kernel] Atom interpolation (`:"foo_#{bar}"`) is deprecated in favor of explicit `String.to_unsafe_atom/1`
  * [List] `List.to_atom/1` is deprecated in favor of `List.to_unsafe_atom/1`
  * [String] `String.to_atom/1` is deprecated in favor of `String.to_unsafe_atom/1`

## v1.20

The CHANGELOG for v1.20 releases can be found [in the v1.20 branch](https://github.com/elixir-lang/elixir/blob/v1.20/CHANGELOG.md).
