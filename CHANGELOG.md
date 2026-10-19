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
  * [Code] Add support for the `:erlc_options` compiler option
  * [Code.Formatter] Add a `:migrate_atom_interpolations` option
  * [File] Accept an open IO device in `File.stat/2`, `File.stat!/2`, `File.regular?/2`, `File.dir?/2`, and `File.exists?/2`
  * [IO.ANSI] Add `bold/0`, `bold_off/0`, `dim/0`, `dim_off/0`, `blink/0`, `invisible/0`, `invisible_off/0`, `strikethrough/0`, `strikethrough_off/0`, `italic_off/0`, `underline_off/0`, `overline/0`, `overline_off/0`, `cursor_forward/0`, `cursor_backward/0`, and `cursor_home/0`, aligning names with Erlang/OTP
  * [Kernel] Improve performance of type constructors and complex intersections
  * [Kernel] Warn on aliases that are never used because a later alias shadows them
  * [Kernel] Warn on binary patterns with segments that are not byte-aligned
  * [Kernel.ParallelCompiler] Add a hint when spawned processes cannot load modules defined during compilation
  * [Keyword] Optimize `Keyword.pop/3`, `Keyword.pop!/2`, and `Keyword.pop_lazy/3`
  * [List] Add `List.to_existing_atom/2` and `List.to_unsafe_atom/1`
  * [MapSet] Optimize `MapSet.symmetric_difference/2` when set sizes differ
  * [Path] Add `Path.safe_join/2`
  * [Protocol] Add the `:skip_consolidated` option to `Protocol.extract_protocols/2`
  * [Protocol] Warn when implementing a protocol for a module that does not define a struct
  * [Registry] Add the `:spawn_opt` option to `Registry.start_link/1`
  * [Registry] Optimize exact key matching in lookups
  * [String] Optimize `String.bag_distance/2`
  * [String] Add `String.to_existing_atom/2` and `String.to_unsafe_atom/1`
  * [URI] Optimize percent-decoding and `URI.to_string/1`

#### ExUnit

  * [ExUnit.Assertions] Add `trace/3` helper
  * [ExUnit.CLIFormatter] Improve rendering of test locations in some terminals

#### Mix

  * [mix test] Add `--coverage-format` to emit coverage reports as `html` (the default) or `text`

### 2. Bug fixes

#### Elixir

  * [Calendar] Fix `Calendar.strftime/3` formatting of negative years with `%y`
  * [Calendar] Fix rounding for `:day`, `:hour`, and `:minute` units in `DateTime.diff/3`, `NaiveDateTime.diff/3`, and `Time.diff/3`
  * [Calendar.ISO] Fix `Calendar.ISO.valid_time?/4` to reject non-integer microsecond precision
  * [Calendar.ISO] Reject negative zero UTC offsets in basic formats
  * [Code.Formatter] Fix rendering calls where `do` is followed by non-block keyword arguments
  * [Code.Formatter] Keep comments written after do-end block keywords (such as `rescue` and `after`) or after the right side of an operator close to where they were written, instead of moving them before the whole expression
  * [Code.Fragment] Fix cursor completion when operator keywords such as `in`, `when`, `and`, `or`, and `not` follow another operator
  * [Date] Preserve the `:format` option in `Date.to_iso8601/2` with custom calendars
  * [Date.Range] Fix slicing date ranges with stepped ranges
  * [DateTime] Keep the calendar of the given map in `DateTime.convert/2` and `DateTime.truncate/2`
  * [DateTime] Truncate fractional differences towards zero in `DateTime.diff/3`
  * [Duration] Reject duplicate seconds in `Duration.from_iso8601/1`
  * [Enum] Fix `Enum.min/2,3` and `Enum.max/2,3` with custom sorters on ranges
  * [IO.ANSI.Docs] Recognize additional punctuation delimiters when rendering Markdown
  * [Kernel] Fix expansion of rebound variables in bitstring size expressions
  * [Kernel] Expand `defguard` macros separately in guard and body contexts, preserving `and`/`or` error semantics outside guards
  * [Kernel] Fix inferred stacktrace types to allow arbitrary keyword metadata
  * [Kernel] Fix inferred types for functions with non-returning clauses
  * [Kernel] Fix map field type inference in the presence of empty map types
  * [Kernel] Fix tuple fetch and deletion type operations across equivalent tuple types
  * [Kernel] Fix variables defined in one default argument leaking into subsequent default arguments
  * [Kernel] Improve the error message for non-atom struct keys
  * [Kernel] Raise when `|` is used in guards
  * [Kernel] Raise on duplicate keys in `@enforce_keys`
  * [Kernel] Fix parsing precedence of unary operators applied to do-end blocks, such as `!if a do b end || c`
  * [Kernel] Do not accept empty tuples in `elem/2` and other tuple operations during type checking
  * [Kernel] Fix inferred types of comprehensions using `:into` with binaries and bitstrings
  * [Kernel] Fix type checker crash on functions with impossible clauses
  * [Kernel] Fix type checker crash on `defimpl` for modules that are not Elixir structs
  * [Kernel] Fix unsound type refinement of `min/2` and `max/2` arguments
  * [Kernel] Improve type precision of `--/2`
  * [Kernel] Improve type precision of `case` when some of its clauses always raise
  * [Kernel] Make pin patterns with singleton types precise, so subsequent redundant clauses are detected
  * [Kernel] Report applying empty function types as bad functions instead of bad arities
  * [Keyword] Delete duplicate keys when `Keyword.get_and_update/3` and `Keyword.get_and_update!/3` return `:pop`
  * [Macro] Properly escape C1 control characters and Unicode noncharacters
  * [Macro] Respect the disabled ANSI setting in `Macro.dbg/3`
  * [Macro] Return `true` from `Macro.quoted_literal?/1` for literals wrapped in `__block__`
  * [NaiveDateTime] Fix `NaiveDateTime.diff/3` over-counting incomplete units
  * [OptionParser] Report missing and invalid arguments for switches with `:keep`
  * [Range] Fix `Range.disjoint?/2` for ranges beyond floating-point precision
  * [Range] Fix `Range.disjoint?/2` for single-element ranges with a negative step
  * [Regex] Escape U+0085, U+200E, U+200F, U+2028, and U+2029 in `Regex.escape/1`, as Erlang/OTP 28+ ignores them in extended mode
  * [String] Fix `String.reverse/1` grapheme ordering around invalid UTF-8 bytes
  * [String] Return `1.0` from `String.bag_distance/2` for two empty strings
  * [Time] Validate microseconds in `Time.from_seconds_after_midnight/3`

#### ExUnit

  * [ExUnit.Assertions] Fix `refute_in_delta/4` at the delta boundary and with negative deltas
  * [ExUnit.CaptureIO] Stop `StringIO` processes when capturing a named device fails
  * [ExUnit.CLIFormatter] Report failures whose messages contain invalid UTF-8
  * [ExUnit.FailuresManifest] Do not overlap failures of sibling parameterized tests
  * [ExUnit.Runner] Give each parameter set of parameterized tests its own random seed
  * [ExUnit.Runner] Preserve the order of async modules across `--repeat-until-failure` runs

#### IEx

  * [IEx.Autocomplete] Fix completion crashes on maps with non-atom keys
  * [IEx.Autocomplete] Do not split Unicode codepoints when completing map keys
  * [IEx.Evaluator] Recognize `**` and `not in` as continuation operators
  * [IEx.Helpers] Fix `r/1` when multiple modules are defined in the same file
  * [IEx.Helpers] Fix heap and stack memory calculations in `process_info/1`
  * [IEx.Helpers] Fix showing macro specs in `h/1`

#### Mix

  * [Mix] Prevent synchronization lock files from being overwritten with empty contents
  * [Mix.Release] Accept chardata paths in `Mix.Release.make_boot_script/4`
  * [Mix.SCM.Git] Raise if Git refspecs start with `-`
  * [mix compile] Compile optional dependencies that non-optional dependencies require when `--no-optional-deps` is given
  * [mix deps] Recompile path and fetchable dependencies when one of the dependencies they were compiled with is removed
  * [mix deps] Mark fetchable dependencies for compilation when their build exists but their SCM manifest is missing
  * [mix deps.compile] Preserve code paths and compiler options across OS partitions
  * [mix format] Pass `:sigils` to plugins invoked for sigils, allowing nested sigils to be formatted
  * [mix local.hex] Install Hex with `--if-missing` when the installed version is older than the requested one
  * [mix new] Avoid trailing whitespace in generated files
  * [mix release] Keep `:erts` in the release applications

### 3. Hard deprecations

#### Elixir

  * [Macro.Env] `Macro.Env.fetch_alias/2` and `Macro.Env.fetch_macro_alias/2` are deprecated, use `Macro.Env.expand_alias/4` instead
  * [Path] `Path.safe_relative_to/2` is deprecated, use `Path.safe_relative/2` instead

### 4. Soft deprecations

#### Elixir

  * [IO.ANSI] `blink_slow/0`, `conceal/0`, `crossed_out/0`, `cursor_left/0`, `cursor_right/0`, `home/0`, `no_underline/0`, `not_crossed_out/0`, `not_italic/0`, `not_overlined/0`, `overlined/0`, `reveal/0`, `reverse/0`, and `reverse_off/0` are deprecated in favor of the names aligned with Erlang/OTP
  * [Kernel] Atom interpolation (`:"foo_#{bar}"`) is deprecated in favor of explicit `String.to_unsafe_atom/1`
  * [List] `List.to_atom/1` is deprecated in favor of `List.to_unsafe_atom/1`
  * [String] `String.to_atom/1` is deprecated in favor of `String.to_unsafe_atom/1`

## v1.20

The CHANGELOG for v1.20 releases can be found [in the v1.20 branch](https://github.com/elixir-lang/elixir/blob/v1.20/CHANGELOG.md).
