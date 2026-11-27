<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.22

## v1.22.0-dev

### 1. Enhancements

#### Elixir

  * [Access] Allow `Access.get_and_update/3` callbacks to return `:skip`, which returns the current value and leaves the container unchanged. This is supported by `Map.get_and_update/3`, `Map.get_and_update!/3`, `Keyword.get_and_update/3`, `Keyword.get_and_update!/3`, and all accessors in the `Access` module

#### Mix

  * [mix deps] Support Gleam dependencies (requires Gleam v1.19+)

### 2. Bug fixes

#### Potential breaking changes

The following bug fixes may break applications relying on the broken behaviour:

  * [Kernel] `pop_in/1` and `pop_in/2` only pop the last entry in the path. Custom `Access` implementations and accessor functions must handle `:skip`, which is returned when the path goes through a `nil` value
  * [Kernel] Preserve the previous selection when `import` is given `:except`, instead of adding entries that were not previously imported
  * [Kernel] Always reset the previous selection when `import` is given `:only`, including `only: :functions`, `only: :macros`, and `only: :sigils` combined with `:except`

#### Mix

  * [Mix] Declare `eex` as a runtime dependency of Mix, so it is no longer pruned from the code path and is included in releases that include `mix`

### 3. Hard deprecations

#### Elixir

  * [Kernel] Passing a non-list as fields to `struct/2` and `struct!/2` is deprecated
  * [Kernel] `unless/2` is deprecated in favor of `if/2` with a negated condition. `mix format --migrate` can automatically rewrite `unless` expressions
  * [Macro] `Macro.struct!/2` is deprecated in favor of `Macro.struct_info!/2`
  * [Protocol] Defining `__deriving__/3` in the `Any` implementation of a protocol is deprecated in favor of defining the `__deriving__/2` macro in the protocol itself
  * [Regex] `Regex.recompile/1` and `Regex.recompile!/1` are deprecated as they have no effect
  * [Regex] `Regex.version/0` is deprecated in favor of `:re.version/0`

## v1.21

The CHANGELOG for v1.21 releases can be found [in the v1.21 branch](https://github.com/elixir-lang/elixir/blob/v1.21/CHANGELOG.md).
