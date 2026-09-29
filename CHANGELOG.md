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
  * [Kernel] Improve performance of type constructors and complex intersections
  * [Kernel] Warn on binary patterns with segments that are not byte-aligned
  * [Kernel.ParallelCompiler] Add a hint when spawned processes cannot load modules defined during compilation
  * [Keyword] Optimize `Keyword.pop/3`, `Keyword.pop!/2`, and `Keyword.pop_lazy/3`
  * [List] Add `List.to_existing_atom/2` and `List.to_unsafe_atom/1`
  * [MapSet] Optimize `MapSet.symmetric_difference/2` when set sizes differ
  * [Path] Add `Path.safe_join/2`
  * [Registry] Optimize exact key matching in lookups
  * [String] Optimize `String.bag_distance/2`
  * [String] Add `String.to_existing_atom/2` and `String.to_unsafe_atom/1`
  * [URI] Optimize percent-decoding and `URI.to_string/1`

#### ExUnit

  * [ExUnit.Assertions] Add `trace/3` helper

### 2. Bug fixes

#### Elixir

  * [Calendar] Fix `Calendar.strftime/3` formatting of negative years with `%y`
  * [Calendar] Fix rounding for `:day`, `:hour`, and `:minute` units in `DateTime.diff/3`, `NaiveDateTime.diff/3`, and `Time.diff/3`
  * [Calendar.ISO] Fix `Calendar.ISO.valid_time?/4` to reject non-integer microsecond precision
  * [Calendar.ISO] Reject negative zero UTC offsets in basic formats
  * [Code.Formatter] Fix rendering calls where `do` is followed by non-block keyword arguments
  * [Code.Fragment] Fix cursor completion when operator keywords such as `in`, `when`, `and`, `or`, and `not` follow another operator
  * [Date] Preserve the `:format` option in `Date.to_iso8601/2` with custom calendars
  * [Date.Range] Fix slicing date ranges with stepped ranges
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
  * [Kernel.Typespec] Preserve metadata when proxying to Elixir typespecs
  * [Keyword] Delete duplicate keys when `Keyword.get_and_update/3` and `Keyword.get_and_update!/3` return `:pop`
  * [Macro] Properly escape C1 control characters and Unicode noncharacters
  * [NaiveDateTime] Fix `NaiveDateTime.diff/3` over-counting incomplete units
  * [Range] Fix `Range.disjoint?/2` for ranges beyond floating-point precision
  * [Range] Fix `Range.disjoint?/2` for single-element ranges with a negative step
  * [String] Fix `String.reverse/1` grapheme ordering around invalid UTF-8 bytes
  * [String] Return `1.0` from `String.bag_distance/2` for two empty strings
  * [Time] Validate microseconds in `Time.from_seconds_after_midnight/3`

#### ExUnit

  * [ExUnit.Assertions] Fix `refute_in_delta/4` at the delta boundary and with negative deltas
  * [ExUnit.CaptureIO] Stop `StringIO` processes when capturing a named device fails

#### IEx

  * [IEx.Autocomplete] Fix completion crashes on maps with non-atom keys
  * [IEx.Evaluator] Recognize `**` and `not in` as continuation operators
  * [IEx.Helpers] Fix `r/1` when multiple modules are defined in the same file
  * [IEx.Helpers] Fix heap and stack memory calculations in `process_info/1`

#### Mix

  * [Mix] Prevent synchronization lock files from being overwritten with empty contents
  * [Mix.Release] Accept chardata paths in `Mix.Release.make_boot_script/4`
  * [Mix.SCM.Git] Raise if Git refspecs start with `-`
  * [mix deps] Recompile path and fetchable dependencies when one of the dependencies they were compiled with is removed
  * [mix deps] Mark fetchable dependencies for compilation when their build exists but their SCM manifest is missing
  * [mix deps.compile] Preserve code paths and compiler options across OS partitions
  * [mix format] Pass `:sigils` to plugins invoked for sigils, allowing nested sigils to be formatted
  * [mix new] Avoid trailing whitespace in generated files

### 3. Hard deprecations

#### Elixir

  * [Macro.Env] `Macro.Env.fetch_alias/2` and `Macro.Env.fetch_macro_alias/2` are deprecated, use `Macro.Env.expand_alias/4` instead

### 4. Soft deprecations

#### Elixir

  * [Kernel] Atom interpolation (`:"foo_#{bar}"`) is deprecated in favor of explicit `String.to_unsafe_atom/1`
  * [List] `List.to_atom/1` is deprecated in favor of `List.to_unsafe_atom/1`
  * [String] `String.to_atom/1` is deprecated in favor of `String.to_unsafe_atom/1`

## v1.20

The CHANGELOG for v1.20 releases can be found [in the v1.20 branch](https://github.com/elixir-lang/elixir/blob/v1.20/CHANGELOG.md).
