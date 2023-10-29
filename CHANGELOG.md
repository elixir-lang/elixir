# Changelog for Elixir v1.16

## v1.17.0-dev

### 1. Enhancements

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

### 4. Hard deprecations

#### Elixir

  * [IO] Passing `:all` to `IO.read/2` and `IO.binread/2` is deprecated, pass `:eof` instead
  * [Kernel] Single-quote charlists are deprecated, use `~c` instead
  * [Range] `left..right` without explicit steps inside patterns and guards is deprecated, write `left..right//step` instead
  * [Range] Decreasing ranges, such as `10..1` without an explicit step is deprecated, write `10..1//-1` instead

#### ExUnit

  * [ExUnit.Case] `register_test/4` is deprecated in favor of `register_test/6` for performance reasons

## v1.15

The CHANGELOG for v1.16 releases can be found [in the v1.16 branch](https://github.com/elixir-lang/elixir/blob/v1.16/CHANGELOG.md).
