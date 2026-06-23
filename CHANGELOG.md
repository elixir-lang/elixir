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
  * [Kernel] Warn on binary patterns with segments that are not byte-aligned
  * [Keyword] Optimize `Keyword.pop/3`, `Keyword.pop!/2`, and `Keyword.pop_lazy/3`
  * [List] Add `List.to_existing_atom/2` and `List.to_unsafe_atom/1`
  * [MapSet] Optimize `MapSet.symmetric_difference/2` when set sizes differ
  * [Registry] Optimize exact key matching in lookups
  * [String] Optimize `String.bag_distance/2`
  * [String] Add `String.to_existing_atom/2` and `String.to_unsafe_atom/1`
  * [URI] Optimize percent-decoding and `URI.to_string/1`

#### ExUnit

  * [ExUnit.Assertions] Add `trace/3` helper

### 2. Bug fixes

#### Elixir

  * [Kernel] Fix expansion of rebound variables in bitstring size expressions
  * [Kernel] Fix variables defined in one default argument leaking into subsequent default arguments
  * [Kernel.Typespec] Preserve metadata when proxying to Elixir typespecs

#### ExUnit

  * [ExUnit.CaptureLog] Fix the typespec for capture log options

#### Mix

  * [Mix.SCM.Git] Raise if Git refspecs start with `-`
  * [mix deps.compile] Preserve code paths and compiler options across OS partitions

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
