# Changelog for Elixir v1.3

## v1.3.0-dev

### 1. Enhancements

#### EEx

  * [EEx.Engine] Support an `init/1` function in engines that will return the initial buffer (defaults to an empty string)

#### Elixir

  * [Calendar] Add `Calendar` and `Date`, `Time`, `NaiveDateTime` and `DateTime` types
  * [CLI] Add `--logger-otp-reports BOOL` and `--logger-sasl-reports BOOL` switches
  * [Compiler] Emit a summary of compilation errors when modules are missing
  * [Enum] Add `Enum.group_by/3` that allows developers to map on the value being grouped
  * [Exception] Suggest possible functions on `UndefinedFunctionError` for existing modules
  * [Exception] Warn if unknown fields are given to `raise/2`
  * [Exception] Warn if fields are missing when calling `raise/2`
  * [File] Support IO devices in `File.copy/3`
  * [GenServer] Raise a more meaningful exit if you try to `GenServer.call/3` yourself
  * [Inspect] Support `:base` option when inspecting binaries
  * [IO] Add `IO.warn/2` that will print a warning message with stacktrace and notify the compiler a warning was printed (in case --warnings-as-errors was enabled)
  * [Kernel] Support `generated: true` in quote
  * [Kernel] Support `Kernel.pop_in/1` and `Kernel.pop_in/2` for yanking a value from a nested data structure
  * [Kernel] Allow variable struct names when matching, for example, `%module{key: "value"} = struct`
  * [Kernel] Allow guards on the left side of `<-` in `for` and `with` special forms
  * [Kernel] Support `else` chunks in `with`
  * [Kernel] Track `{module, function, arity}` imports and warn on unused ones when such are specified in `:only`
  * [Kernel] Add `keyword/0` and `keyword/1` built-in types to typespecs
  * [OptionParser] Add support for `:count` switch type
  * [OptionParser] Add `parse!/2` and `parse_head!/2` that raise `OptionParser.ParseError` in case of errors
  * [Process] Add `Process.sleep/1`
  * [Range] `Range.range?/1` now checks the validity of a range.
  * [Regex] Support `:include_captures` in `Regex.split/3`
  * [String] Add `String.myers_difference/2` for calculating the difference between two strings
  * [System] Add `System.os_time/0` and `System.os_time/1`
  * [URI] Add `URI.merge/2`
  * [Version] Add `Version.parse!/1`

#### ExUnit

  * [ExUnit] Show pinned variables on failed `assert ^left = right` and `assert match?(^left, right)` assertions
  * [ExUnit] Add `ExUnit.Case.register_attribute` which allow attributes to be cleaned up whenever a test is defined
  * [ExUnit] Add `ExUnit.Case.register_test` and support the ability to tag "tests" by type. This will allow projects like QuickCheck to change the wording in formatters to say "10 properties" instead of "10 tests"
  * [ExUnit] Support diffing of values when using `==` in `assert`
  * [ExUnit] Start running tests as soon as cases are loaded. This feature is enabled by default when running tests through Mix
  * [ExUnit] Raise a straight-forward error message in case a duplicate test name is defined

#### IEx

  * [IEx] Add `nl/2` that loads a given modules on a list of nodes
  * [IEx.Helpers] No longer restart applications on `recompile/1`
  * [IEx.Autocomplete] Improve IEx expand to handle functions after `&`

#### Logger

  * [Logger] Introduce `Logger.reset_metadata/0,1`

#### Mix

  * [Mix] Add `mix xref` and `mix compile.xref` that runs cross-reference checks, with the latter running after compilation by default
  * [Mix] Add `mix app.tree` and `mix deps.tree`
  * [Mix] Add `Mix.Task.rerun/2` that reenables and re-runs a task
  * [Mix] Integrate `OptionParser.ParseError` into Mix, automatically converting such exceptions into `Mix.Error` and embedding the task information
  * [Mix] Support `@preferred_cli_env` attribute when defining tasks
  * [Mix] Support `mix test --raise` that will raise when a test suite fails (instead of setting the exit code to 1)
  * [Mix] Enable rebar3 manager by default for Hex dependencies
  * [Mix] Add `mix escript.install` to install escripts
  * [Mix] Print stacktraces for `Mix.Error` when `MIX_DEBUG=1` is set
  * [Mix] Add a user friendly error for merge conflicts on `mix.lock`
  * [Mix] Track files between path dependencies. This means umbrella applications will no longer trigger full recompilation when a sibling changes. Instead it will only recompile the files affected by the sibling changes
  * [Mix] No longer print every file being compiled. Instead a generic "Compiling N files (.ext)" will be printed and files will only be logged in case they take more than 5 seconds to compile. This threshold can be customized by passing the `--long-compilation-threshold` flag and the previous behaviour can be reenabled by giving `--verbose` to `mix compile`

### 2. Bug fixes

#### Elixir

  * [Application] Ensure `Application.spec/2` returns nil for unknown applications
  * [GenServer] Ensures `cast/2` returns `:ok` if locally registered process is not found
  * [Inspect] Ensure binaries break into new lines when inspected
  * [Kernel] Do not choke on capture operator with argument above `&191`
  * [Kernel] Raise if `defstruct` is called multiple times
  * [Kernel] Ensure `Module.create/3` respects var/alias hygiene
  * [Kernel] Support non-literal ranges on the right side of `in/2`
  * [OptionParser] Allow `OptionParser` to parse negative numbers
  * [Macro] Fix `Macro.to_string/1` on a call of a capture argument, for example `&(&1).(:x)`
  * [String] Ensure `strip` also removes non-breaking whitespaces (and ensure `split` still does not split on them)

#### Mix

  * [Mix] Improve task not found message when Mix would include the not found task as a suggestion due to different casing
  * [Mix] Ignore lock revision when the lock is out of date when updating Mix dependencies. Before this fix, git tags and branches in the lock file would erroneously take higher precedence than the one in `mix.exs`
  * [Mix] Only recompile empty Elixir files if they change instead of recompiling them on every run
  * [Mix] Ensure .app file is written in UTF-8 (this allows app descriptions to contain UTF-8 characters)
  * [Mix.Dep] Always specify the `:env` option internally for dependencies to avoid false positives in the dependency resolution
  * [Mix.Dep] Correctly detect conflict from cousin optional dependencies in the dependency resolution algorithm

### 3. Soft deprecations (no warnings emitted)

  * [Float] `Float.to_string/2` and `Float.to_char_list/2` has been soft-deprecated as Elixir will now attempt to print the shortest and most accurate representation by default. Developers can always fallback to `:erlang.float_to_binary/2` and `:erlang.float_to_list/2` if they need the previous functionality
  * [String] The confusing `String.strip/2`, `String.lstrip/2` and `String.rstrip/2` API has been soft deprecated in favor of `String.trim/2`, `String.trim_leading/2` and `String.trim_trailing/2`
  * [String] The confusing `String.ljust/3` and `String.rjust/3` API has been soft deprecated in favor of `String.pad_leading/3` and `String.pad_trailing/3`

### 4. Deprecations

This release deprecates many APIs that have been soft-deprecated in previous Elixir versions.

#### Elixir

  * [Dict] `Dict` is no longer a behaviour and its functions will be deprecated in upcoming releases
  * [Enum] Passing a dictionary to `Enum.group_by/3` is deprecated
  * [Kernel] `\x{H*}` in strings/sigils/charlists is deprecated
  * [Kernel] Add deprecation for `defdelegate` list arguments and `:append_first` option
  * [Kernel] Warn if a variable is assigned inside `case`/`if`/etc and used outside the block
  * [Keyword] `Keyword.size/1` is deprecated in favor of `Kernel.length/1`
  * [Map] `Map.size/1` is deprecated in favor of `Kernel.map_size/1`
  * [Regex] The option `/r` (for ungreedy) has been deprecated in favor of `/U`
  * [Set] `Set` is no longer a behaviour and its functions will be deprecated in upcoming releases
  * [String] `String.valid_character?/1` is deprecated in favor of `String.valid?/1` with pattern matching
  * [Task] `Task.find/2` is deprecated in favor of explicit message matching
  * [URI] Passing a non-map to `URI.decode_query/3` is deprecated
