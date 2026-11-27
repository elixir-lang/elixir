<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.22

## v1.22.0-dev

### 1. Enhancements

### 2. Bug fixes

### 3. Hard deprecations

#### Elixir

  * [Kernel] `unless/2` is deprecated in favor of `if/2` with a negated condition. `mix format --migrate` can automatically rewrite `unless` expressions
  * [Macro] `Macro.struct!/2` is deprecated in favor of `Macro.struct_info!/2`
  * [Protocol] Defining `__deriving__/3` in the `Any` implementation of a protocol is deprecated in favor of defining the `__deriving__/2` macro in the protocol itself
  * [Regex] `Regex.recompile/1` and `Regex.recompile!/1` are deprecated as they have no effect
  * [Regex] `Regex.version/0` is deprecated in favor of `:re.version/0`

## v1.21

The CHANGELOG for v1.21 releases can be found [in the v1.21 branch](https://github.com/elixir-lang/elixir/blob/v1.21/CHANGELOG.md).
