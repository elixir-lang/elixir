# Changelog for Elixir v1.4

## v1.4.0-dev

### 1. Enhancements

#### Elixir

  * [Enum] Add `Enum.map_every/2` that invokes the given function with every nth item
  * [Integer] `Integer.digits/2` now accepts negative integers
  * [Integer] Add `Integer.mod/2` and `Integer.floor_div/2`
  * [List] Add `List.pop_at/3`
  * [Stream] Add `Stream.map_every/2` that invokes the given function with every nth item

#### IEx

  * [IEx.Helpers] `c/1` now compiles in memory by default to avoid common issue where `.beam` files remain at projects root directory
  * [IEx.Autocomplete] Stop appending a trailing dot when autocompleting modules in IEx
  * [IEx.Autocomplete] Support autocompletion for structs
  * [IEx.Server] Support interrupting IEx evaluation through the Ctrl+G prompt

#### Mix

  * [Mix] Add warning for invalid paths on `mix deps.clean`
  * [Mix] Provide "did you mean?" suggestions for `mix xref`
  * [Mix.Rebar] Add `MIX_REBAR` environment variable for overriding local rebar

#### ExUnit

  * [ExUnit.Doctest] Allow inspected structures with multiples lines and unicode characters in the doctest result

### 2. Bug fixes


### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Enum] `Enum.partition/2` has been deprecated in favor of `Enum.split_with/2`
  * [Stream] Deprecate `Stream.uniq/2` in favor of `Stream.uniq_by/2` (to mirror the `Enum` API)

### 4. Deprecations

#### Elixir

  * [Kernel] Deprecate support for making private functions overridable. Overridable functions must always be public as they must be contracts.

#### IEx

  * [IEx.Helpers] `import_file/2` is deprecated in favor of `import_file_if_available/1`

