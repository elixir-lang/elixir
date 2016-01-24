# Changelog for Elixir v1.3

## v1.3.0-dev

### 1. Enhancements

#### EEx

  * [EEx.Engine] Support an `init/1` function in engines that will return the initial buffer (defaults to an empty string)

#### Elixir

  * [File] Support IO devices in `File.copy/3`
  * [Compiler] Emit a summary of compilation errors when modules are missing
  * [Regex] Support `:include_captures` in `Regex.split/3`

#### ExUnit

  * [ExUnit] Show pinned variables on failed `assert ^left = right` and `assert match?(^left, right)` assertions

#### IEx

  * [IEx.Autocomplete] Improve IEx expand to handle functions after `&`

#### Logger

  * [Logger] Introduce `Logger.reset_metadata/0,1`

#### Mix

### 2. Bug fixes

#### Elixir

  * [Application] Ensure `Application.spec/2` returns nil for unknown applications
  * [Inspect] Ensure binaries break into new lines when inspected
  * [Kernel] Do not choke on capture operator with argument above `&191`
  * [Kernel] Raise if `defstruct` is called multiple times
  * [Macro] Fix `Macro.to_string/1` on a call of a capture argument, for example `&(&1).(:x)`

#### Mix

  * [Mix] Improve task not found message when Mix would include the not found task as a suggestion due to different casing

### 3. Soft deprecations (no warnings emitted)

### 4. Deprecations

This release deprecates many APIs that have been soft-deprecated in previous Elixir versions.

#### Elixir

  * [Dict] `Dict` is no longer a behaviour and its functions will be deprecated in upcoming releases
  * [Enum] Passing a non-map to `Enum.group_by/3` is deprecated
  * [Kernel] `\x{H*}` in strings/sigils/char lists is deprecated
  * [Kernel] Add deprecation for `defdelegate` list arguments and `:append_first` option
  * [Keyword] `Keyword.size/1` is deprecated in favor of `Kernel.length/1`
  * [Map] `Map.size/1` is deprecated in favor of `Kernel.map_size/1`
  * [Regex] The option `/r` (for ungreedy) has been deprecated in favor of `/U`
  * [Set] `Set` is no longer a behaviour and its functions will be deprecated in upcoming releases
  * [String] `String.valid_character?/1` is deprecated in favor of `String.valid?/1` with pattern matching
  * [Task] `Task.find/2` is deprecated in favor of explicit message matching
  * [URI] Passing a non-map to `URI.decode_query/3` is deprecated
