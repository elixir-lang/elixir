# Changelog for Elixir v1.12

## v1.12.0-dev

### 1. Enhancements

#### EEx

  * [EEx.Engine] Add `c:EEx.Engine.handle_text/3` callback that receives text metadata
  * [EEx.Engine] Emit warnings for unused "do" expression in EEx

#### Elixir

  * [Code] Do not add newlines around interpolation on code formatting. Note this means formatted code that has interpolation after the line length on Elixir v1.12 won't be considered as formatted on earlier Elixir versions
  * [Calendar] Support basic datetime format in `Calendar.ISO` parsing functions
  * [Code] Improve evaluation performance on systems running on Erlang/OTP 24+
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
  * [Kernel] Improve error messages on invalid character right after a number
  * [Kernel] Show removal and deprecated tips from Erlang/OTP
  * [Macro] Add export dependencies on `Macro.struct!/2`
  * [Macro] Support `:newline` to customize newlines escaping in `Macro.unescape_string/2`
  * [Module] Raise on invalid `@dialyzer` attributes
  * [Module] Add `Module.get_definition/2` and `Module.delete_definition/2`
  * [Module] Allow `@on_load` to be a private function
  * [Regex] Add offset option to `Regex.scan/3` and `Regex.run/3`
  * [Registry] Support compression on `Registry` tables
  * [Stream] Add `Stream.zip_with/2` and `Stream.zip_with/3`
  * [String] Add `:turkic` mode option to String case functions
  * [System] Add `System.trap_signal/3` and `System.untrap_signal/2`
  * [Tuple] Add `Tuple.sum/1` and `Tuple.product/1`
  * [URI] Add functions for RFC3986 compliant encoding and decoding of queries

#### ExUnit

  * [ExUnit] Intercept SIGQUIT and show a list of all aborted tests as well as intermediate test results
  * [ExUnit] Interpolate module attributes in match assertions diffs
  * [ExUnit] Print how much time is spent on `async` vs `sync` tests
  * [ExUnit] Improve error messages for doctests
  * [ExUnit] Compile doctests faster (often by two times)

#### IEx

  * [IEx] Make IEx' parser configurable to allow special commands
  * [IEx] Show function signature when pressing tab after the opening parens of a function
  * [IEx] If an IEx expression starts with a binary operator, such as `|>`, automatically pipe in the result of the last expression

#### Mix

  * [Mix] Add `Mix.install/2` for dynamically installing a list of dependencies
  * [Mix] Support `:exit_code` option in `Mix.raise/2`
  * [Mix] Discard `MIX_ENV` and `MIX_TARGET` values if they are empty strings
  * [Mix] Print the time taken to execute a task with on `MIX_DEBUG=1`
  * [mix escript.build] Deep merge configuration and ensure argv is set when executing `config/runtime.exs`
  * [mix release] Add `RELEASE_PROG` to releases with the name of the executable starting the release

### 2. Bug fixes

#### Elixir

  * [CLI] Ensure `-e ""` (with an empty string) parses correctly on Windows
  * [Inspect] Do not override user supplied `:limit` option for derived implementations
  * [Kernel] Allow heredoc inside a heredoc interpolation
  * [Kernel] Preserve CRLF on heredocs
  * [Kernel] Public functions without documentation now appear as an empty map on `Code.fetch_docs/1`, unless they start with underscore, where they remain as `:none`. This aligns Elixir's implementation with EEP48
  * [Kernel] Do not crash when complex literals (binaries and maps) are used in guards
  * [Kernel] Properly parse keywords (such as `end`) followed by the `::` operator
  * [Macro] `Macro.decompose_call/1` now also consider tuples with more than 2 elements to not be valid calls
  * [Macro] Fix `Macro.underscore/1` for digit preceded by capitals: "FOO10" now becomes "foo10" instead of "fo_o123"
  * [OptionParser] Properly parse when numbers follow-up aliases, for example, `-ab3` is now parsed as `-a -b 3`
  * [Path] Fix `Path.relative_to/2` when referencing self
  * [Task] Ensure `Task.async_stream/2` with `ordered: false` discard results as they are emitted, instead of needlessly accumulating inside the stream manager
  * [URI] Do not discard empty paths on `URI.merge/2`

#### IEx

  * [IEx] Fix auto-completion inside remote shells

#### Mix

  * [mix compile.elixir] Ensure that a manifest is generated even with no source code
  * [mix compile.elixir] Make sure export dependencies trigger recompilation when removed
  * [mix compile.elixir] Do not emit false positive warnings when a path dependency adds a module that is then used by the current application
  * [mix test] Ensure protocols within the current project are consolidated when `--cover` is given
  * [mix release] Improve compliance of release scripts with stripped down Linux installations
  * [mix release] Preserve file mode when copying non-beam ebin files

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
