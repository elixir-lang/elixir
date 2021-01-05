# Changelog for Elixir v1.12

## v1.12.0-dev

### 1. Enhancements

#### EEx

  * [EEx.Engine] Add `c:EEx.Engine.handle_text/3` callback that receives text metadata
  * [EEx.Engine] Emit warnings for unused "do" expression in EEx

#### Elixir

  * [Code] Do not add newlines around interpolation on code formatting. Note this means formatted code that has interpolation after the line length on Elixir v1.12 won't be considered as formatted on earlier Elixir versions
  * [DateTime] Add `offset` to `DateTime.to_iso8601/2` (now `to_iso8601/3`)
  * [Enum] Add `Enum.count_until/2` and `Enum.count_until/3`
  * [Enum] Add `Enum.product/1`
  * [Enum] Add `Enum.zip_with/2` and `Enum.zip_with/3`
  * [Enum] Add support for functions as the second argument of `Enum.with_index/2`
  * [Float] Add `Float.pow/2`
  * [Integer] Add `Integer.pow/2`
  * [List] Add default value for `List.first/1` and `List.last/1`
  * [Kernel] Also warn for literal structs on `min/2` and `max/2`
  * [Kernel] Add `Kernel.tap/2` and `Kernel.then/2`
  * [Kernel] Do not add runtime dependencies to remotes in typespecs
  * [Kernel] When there is an unused variable warning and there is a variable with the same name previously defined, suggest the user may have wanted to use the pin operator
  * [Module] Add `Module.get_definition/2` and `Module.delete_definition/2`
  * [Module] Allow `@on_load` to be a private function
  * [Regex] Add offset option to `Regex.scan/3` and `Regex.run/3`
  * [Stream] Add `Stream.zip_with/2` and `Stream.zip_with/3`
  * [String] Add `:turkic` mode option to String case functions
  * [Tuple] Add `Tuple.sum/1` and `Tuple.product/1`

#### ExUnit

  * [ExUnit] Intercept SIGQUIT and show a list of all aborted tests as well as intermediate test results

#### IEx

  * [IEx] Make IEx' parser configurable to allow special commands
  * [IEx] Show function signature when pressing tab after the opening parens of a function

#### Mix

  * [Mix] Support `:exit_code` option in `Mix.raise/2`
  * [Mix] Discard `MIX_ENV` and `MIX_TARGET` values if they are empty strings

### 2. Bug fixes

#### Elixir

  * [CLI] Ensure `-e ""` (with an empty string) parses correctly on Windows
  * [Kernel] Preserve CRLF on heredocs
  * [Kernel] Public functions without documentation now appear as an empty map on `Code.fetch_docs/1`, unless they start with underscore, where they remain as `:none`. This aligns Elixir's implementation with EEP48
  * [Kernel] Do not crash when complex literals (binaries and maps) are used in guards
  * [Macro] `Macro.decompose_call/2` now also consider tuples with more than 2 elements to not be valid calls
  * [OptionParser] Properly parse when numbers follow-up aliases, for example, `-ab3` is now parsed as `-a -b 3`
  * [Path] Fix `Path.relative_to/2` when referencing self

#### IEx

  * [IEx] Fix auto-completion inside remote shells

#### Mix

  * [mix compile.elixir] Ensure that a manifest is generated even with no source code
  * [mix compile.elixir] Make sure export dependencies trigger recompilation when removed

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### EEx

  * [EEx.Engine] `use EEx.Engine` is deprecated in favor of explicit delegation

#### Elixir

  * [Kernel] Deprecate `@foo()` in favor of `@foo`
  * [System] Deprecate `System.stacktrace/0` (it was already deprecated outside of catch/rescue and now it is deprecated everywhere)

#### Mix

  * [mix compile] The `:xref` compiler is deprecated and it has no effect. Please remove it from your mix.exs file.

## v1.11

The CHANGELOG for v1.11 releases can be found [in the v1.11 branch](https://github.com/elixir-lang/elixir/blob/v1.11/CHANGELOG.md).
