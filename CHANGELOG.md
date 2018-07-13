# Changelog for Elixir v1.8

## v1.8.0-dev

### 1. Enhancements

#### Elixir

#### ExUnit

#### IEx

#### Logger

#### Mix

### 2. Bug fixes

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
  * [System] `:seconds`, `:milliseconds`, etc as time units is deprecated in favor of `:second`, `:millisecond`, etc

#### Mix

  * [mix compile.erlang] Returning `{:ok, contents}` or `:error` as the callback in `Mix.Compilers.Erlang.compile/6` is deprecated in favor of returning `{:ok, contents, warnings}` or `{:error, errors, warnings}`

## v1.7

The CHANGELOG for v1.7 releases can be found [in the v1.7 branch](https://github.com/elixir-lang/elixir/blob/v1.7/CHANGELOG.md).
