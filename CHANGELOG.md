# Changelog for Elixir v1.4

## v1.4.0-dev

### 1. Enhancements

#### Elixir

  * [Calendar] Add `Date.compare/2`, `Time.compare/2`, `NaiveDateTime.compare/2` and `DateTime.compare/2`
  * [Calendar] Support `NaiveDateTime.add/3` and `NaiveDateTime.diff/3` for adding seconds (up to microseconds) as well as the difference between two NaiveDateTimes in seconds (up to microseconds)
  * [Enum] Add `Enum.map_every/2` that invokes the given function with every nth item
  * [Enum] Add `min/2`, `max/2`, `min_max/2`, `min_by/3`, `max_by/3`, and `min_max_by/3` that allow a function specifying the default value when the enumerable is empty
  * [Enum] Introduce `Enum.zip/1` to zip multiple entries at once
  * [Float] Introduce `Float.ratio/1` that returns a tuple with the numerator and denominator to retrieve the given float
  * [GenServer] Log error on default `handle_info/2` implementation
  * [Integer] `Integer.digits/2` now accepts negative integers
  * [Integer] Add `Integer.mod/2` and `Integer.floor_div/2`
  * [Logger] Functions passed to `Logger.bare_log/3` can now return a tuple containing additional metdata `{String.t, Keyword.t}`. Through delegation, `Logger.debug/2`, `Logger.info/2`, `Logger.warn/2`, and `Logger.error/2` also respect the new return signature.
  * [List] Add `List.pop_at/3`
  * [List] Add `List.myers_difference/2`
  * [OptionParser] Expand multi-letter aliases in `OptionParser`
  * [Process] Add `Process.send_after/4`
  * [Process] Improve error messages on `Process.register` errors
  * [Stream] Add `Stream.map_every/2` that invokes the given function with every nth item
  * [Stream] Introduce `Stream.zip/1` to lazily zip multiple entries at once
  * [Task] Add `Task.async_stream/3` and `Task.async_stream/5` as well as the supervised versions `Task.Supervisor.async_stream/4` and `Task.Supervisor.async_stream/6`
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

  * [Mix] Provide "did you mean?" suggestions for `mix xref`
  * [Mix] Add the ability to specify one or more apps in `mix cmd`
  * [Mix] Compress archive files built by `mix archive` as they are now unzipped during installation
  * [Mix] Check directory existence in `mix new` and ask how to proceed if one exists
  * [Mix] Applications built with the `--sup` flag now have an individual module to work as application callback
  * [Mix] Add `--formatter` option to `mix test`
  * [Mix.Dep] Add warning for invalid paths on `mix deps.clean`
  * [Mix.Project] Add `Mix.Project.apps_paths` that returns the paths to children applications in umbrella projects
  * [Mix.Rebar] Add `MIX_REBAR` environment variable for overriding local rebar

### 2. Bug fixes

#### Elixir

  * [Float] Avoid multiple roundings in `Float.{ceil/2, floor/2, round/2}`
  * [Kernel] Don't crash in `macro_exported?/3` when dealing with Erlang modules
  * [Kernel.SpecialForms] Produce meaningful warning when with's else clauses have no effect
  * [Macro] Wrap fn calls in parens in `Macro.to_string/2`
  * [Macro] Do not print aliases as keys inside keyword lists in `Macro.to_string/2`
  * [Stream] Ensure `Stream.take/2` does not consume next element on `:suspend`
  * [String] Fix infinite recursion in `String.replace_leading/3` and `String.replace_trailing/3` when given an empty string
  * [Task] Fix `Task.shutdown/1,2` infinite block when task has no monitor

#### ExUnit

  * [ExUnit] Fix a race condition in `assert_receive` where we would assert a message was not received but show it in the list of messages when the message is delivered right after the timeout value

### Mix

  * [Mix.Dep] Use `gmake` on FreeBSD instead of `make` when compiling make dependencies
  * [Mix.Project] Only copy files from source when they're newer than destination (for Windows machines)
  * [Mix.Task] Ensure non-recursive tasks inside umbrella are reenabled

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Enum] `Enum.partition/2` has been deprecated in favor of `Enum.split_with/2`
  * [System] Deprecate plural time units in favor of singular ones to align with future Erlang releases

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

## v1.3

The CHANGELOG for v1.3 releases can be found [in the v1.3 branch](https://github.com/elixir-lang/elixir/blob/v1.3/CHANGELOG.md).
