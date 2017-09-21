# Changelog for Elixir v1.6

## v1.6.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Allow markers "/" and "|" to be used in a custom EEx engine

#### Elixir

  * [Inspect] Add `:strict` and `:flex` breaks to `Inspect.Algebra`
  * [Stream] Add `Stream.intersperse/2`
  * [String] Update to Unicode 10
  * [String] Allow passing empty string `match` to `String.replace/4`

#### Mix

  * [mix compile] Create behavior for Mix compiler tasks

### 2. Bug fixes

#### Elixir

  * [Kernel] Validate variable struct name is atom when used in pattern matching

### 3. Soft deprecations (no warnings emitted)

  * [Inspect.Algebra] `surround/3` and `surround_many/6` are deprecated in favor of `container_doc/6`
  * [Kernel.ParallelCompiler] `files/2` and `files_to_path/3` are deprecated in favor of `compile/2` and `compile_to_path/3`
  * [Kernel.ParallelRequire] `files/2` is deprecated in favor of `Kernel.ParallelCompiler.require/2`

### 4. Deprecations

#### Elixir

  * [Enum] `Enum.partition/2` is deprecated in favor of `Enum.split_with/2`

## v1.5

The CHANGELOG for v1.5 releases can be found [in the v1.5 branch](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md).
