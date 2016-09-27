# Changelog for Elixir v1.4

## v1.4.0-dev

### 1. Enhancements

#### Elixir

  * [Enum] Add `Enum.map_every/2` that invokes the given function with every nth item
  * [Enum] Add `min/2`, `max/2` and `min_max/2` that allow a function specifying the default value to `Enum`
  * [Integer] `Integer.digits/2` now accepts negative integers
  * [Integer] Add `Integer.mod/2` and `Integer.floor_div/2`
  * [List] Add `List.pop_at/3`
  * [OptionParser] Expand multi-letter aliases in OptionParser
  * [Process] Add `Process.send_after/4`
  * [Stream] Add `Stream.map_every/2` that invokes the given function with every nth item
  * [URI] Allow 0 as URI scheme default port

#### ExUnit

  * [ExUnit.Doctest] Allow inspected structures with multiples lines and unicode characters in the doctest result
  * [ExUnit.Formatter] Replace lhs/rhs with left/right in the formatter for clarity

#### IEx

  * [IEx.Helpers] `c/1` now compiles in memory by default to avoid common issue where `.beam` files remain at projects root directory
  * [IEx.Helpers] Add info about protocols in `i/1`
  * [IEx.Autocomplete] Stop appending a trailing dot when autocompleting modules in IEx
  * [IEx.Autocomplete] Support autocompletion for structs
  * [IEx.Server] Support interrupting IEx evaluation through the Ctrl+G prompt

#### Mix

  * [Mix] Add warning for invalid paths on `mix deps.clean`
  * [Mix] Provide "did you mean?" suggestions for `mix xref`
  * [Mix] Add the ability to specify one or more apps in `mix cmd`
  * [Mix] Compress archive files built by `mix archive` as they are now unzipped during installation
  * [Mix.Rebar] Add `MIX_REBAR` environment variable for overriding local rebar

### 2. Bug fixes

#### Elixir

  * [Kernel] Don't crash in `macro_exported?/3` when dealing with Erlang modules
  * [Kernel.SpecialForms] Produce meaningful warning when with's else clauses have no effect
  * [Macro] Wrap fn calls in parens in `Macro.to_string/1`
  * [Stream] Ensure `Stream.take/2` does not consume next element on `:suspend`
  * [String] Fix infinite recursion in `String.replace_leading/3` and `String.replace_trailing/3` when given an empty string
  * [Task] Fix `Task.shutdown/1,2` infinite block when task has no monitor

#### ExUnit

  * [ExUnit] Fix a race condition in `assert_receive` where we would assert a message was not received but show it in the list of messages when the message is delivered right after the timeout value

### Mix

  * [Mix.Project] Only copy files from source when they're newer than destination (for Windows machines)

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Enum] `Enum.partition/2` has been deprecated in favor of `Enum.split_with/2`

### 4. Deprecations

#### Elixir

  * [Behaviour] The `Behaviour` module is deprecated. Callbacks may now be defined directly via the `@callback` attribute
  * [Enum] Deprecate `Enum.uniq/2` in favor of `Enum.uniq_by/2`
  * [Float] `Float.to_char_list/2` and `Float.to_string/2` are deprecated (use the :erlang functions if such conversions are desired)
  * [Kernel] Deprecate support for making private functions overridable. Overridable functions must always be public as they must be contracts
  * [Kernel] Warn if variable is used as a function call
  * [OptionParser] Deprecate aliases with multiple letters, such as `-abc`
  * [Stream] Deprecate `Stream.uniq/2` in favor of `Stream.uniq_by/2`

#### IEx

  * [IEx.Helpers] `import_file/2` is deprecated in favor of `import_file_if_available/1`

#### Mix

  * [Mix.Utils] `underscore/1` and `camelize/1` are deprecated
