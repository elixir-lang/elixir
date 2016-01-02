# Changelog for Elixir v1.3

## v1.3.0-dev

### 1. Enhancements

#### Elixir

#### ExUnit

#### IEx

#### Logger

#### Mix

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

### 4. Deprecations

This release deprecates many APIs that have been soft-deprecated in previous Elixir versions.

#### Elixir

* [Dict] `Dict` is no longer a behaviour and its functions will be deprecated in upcoming releases
* [Enum] Passing a non-map to `Enum.group_by/3` is deprecated
* [Kernel] `\x{H*}` in strings/sigils/char lists is deprecated
* [Keyword] `Keyword.size/1` is deprecated in favor of `Kernel.length/1`
* [Map] `Map.size/1` is deprecated in favor of `Map.size/1`
* [Regex] The option `/r` (for ungreedy) has been deprecated in favor of `/U`
* [Set] `Set` is no longer a behaviour and its functions will be deprecated in upcoming releases
* [String] `String.valid_character?/1` is deprecated in favor of `valid?/1` with pattern matching
* [Task] `Task.find/2` is deprecated in favor of explicit message matching
* [URI] Passing a non-map to `URI.decode_query/3` is deprecated
