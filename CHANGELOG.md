# Changelog for Elixir v1.6

## v1.6.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Allow markers "/" and "|" to be used in a custom EEx engine

#### Elixir

  * [Inspect] Add `:strict` and `:flex` breaks to `Inspect.Algebra`
  * [Stream] Add `Stream.intersperse/2`
  * [String] Update to Unicode 10
  * [String] Allow using empty string as `match` argument to `String.replace_leading` and `String.replace_tailing`

#### Mix

  * [mix compile] Create behavior for Mix compiler tasks

### 2. Bug fixes

#### Elixir

  * [Kernel] Validate variable struct name is atom when used in pattern matching

### 3. Soft deprecations (no warnings emitted)

### 4. Deprecations

#### Elixir

  * [Enum] `Enum.partition/2` is deprecated in favor of `Enum.split_with/2`

## v1.5

The CHANGELOG for v1.5 releases can be found [in the v1.5 branch](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md).
