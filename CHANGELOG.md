# Changelog for Elixir v1.3

Elixir v1.3 brings many improvements to the language, the compiler and its tooling, specially Mix (Elixir's build tool) and ExUnit (Elixir's test framework).

## Language improvements

The language has been improved semantically and includes new types and APIs. Let's see the three major features.

### Deprecation of imperative assignment

Elixir will now warn if constructs like `if`, `case` and friends rebind a variable that is accessed in an outer scope. As an example, imagine a function called `format` that receives a message and some options and it must return a path alongside the message:

```elixir
def format(message, opts) do
  path =
    if (file = opts[:file]) && (line = opts[:line]) do
      relative = Path.relative_to_cwd(file)
      message  = Exception.format_file_line(relative, line) <> " " <> message
      relative
    end

  {path, message}
end
```

The `if` block above is implicitly rebinding the outer scope variable `message` to a new value. Now imagine we want to move the `if` block to its own function to clean up the implementation:

```elixir
def format(message, opts) do
  path = with_file_and_line(message, opts)
  {path, message}
end

defp with_file_and_line(message, opts) do
  if (file = opts[:file]) && (line = opts[:line]) do
    relative = Path.relative_to_cwd(file)
    message  = Exception.format_file_line(relative, line) <> " " <> message
    relative
  end
end
```

The refactored version is broken because the `if` block was actually returning two values, the relative path *and* the new message. Elixir v1.3 will warn on such cases, forcing both variables to be explicitly returned from `if`, `case` and other constructs. Furthermore, this change gives us the opportunity to unify the language scoping rules in future releases.

### Calendar types and sigils

Elixir v1.3 introduces the `Calendar` module as well as 4 new calendar types:

  * `Date` - used to store dates (year, month, day) in a given calendar
  * `Time` - used to store time (hour, minute, second, microseconds)
  * `NaiveDateTime` - used to store datetimes without a timezone (year, month, day, hour, minute, second, microseconds) in a given calendar. It is called naïve because without a timezone, the datetime may not actually exist. For example, when there are daylight savings changes, a whole hour may not exist (when the clock moves forward) or a particular instant may happen twice (when the clock moves backwards)
  * `DateTime` - used to store datetimes with timezone (year, month, day, hour, minute, second, microsecond and time zone, with abbreviation, UTC and standard offset)

The current Calendar modules and its types is to provide a base for interoperatibility in the ecosystem instead of full-featured datetime API. This release includes basic functionality for building new types and converting them from and back strings.

Elixir v1.3 also introduces 3 new sigils related to the types above:

  * `~D[2016-05-29]` - builds a new date
  * `~T[08:00:00]` and `~T[08:00:00.285]` - builds a new time (with different precisions)
  * `~N[2016-05-29 08:00:00]` - builds a naive date time

### Access selectors

This release introduces new accessors to make it simpler for developers to traverse nested data structures, traversing and updating data in different ways.  For instance, given a user with a list of languages, here is how to deeply traverse the map and convert all language names to uppercase:

```iex
iex> user = %{name: "john",
...>          languages: [%{name: "elixir", type: :functional},
...>                      %{name: "c", type: :procedural}]}
iex> update_in user, [:languages, Access.all(), :name], &String.upcase/1
%{name: "john",
  languages: [%{name: "ELIXIR", type: :functional},
              %{name: "C", type: :procedural}]}
```

You can see the new accessors in the `Access` module.

## Mix

Mix includes new tasks to improve your everyday workflow. Some of those tasks relies on many compiler improvements to know more about your code, providing static analysis to find possible bugs in your code and faster compilation cycles.

### Compiling n files

Mix no longer announces every file it compiles. Instead it outputs how many files there is to compile per compilers. Here is the output for a project like [`gettext`](https://github.com/elixir-lang/gettext):

```
Compiling 1 file (.yrl)
Compiling 1 file (.erl)
Compiling 19 files (.ex)
Generated gettext app
```

In case a file is taking too long to compile, Mix will announce such, for example:

```
Compiling lib/gettext.ex (it's taking more than 5s)
```

The goal of these changes is to put an increased focus on the "warnings" emitted by the compiler.

In any case, the previous behaviour can be brought back with the `--verbose` flag and the compilation threshold for files that are taking long can be set via the `--long-compilation-threshold` option.

### mix xref

Speaking about warnings, Mix v1.3 includes a new task called `xref` that performs cross reference checks in your code. One of such checks is the ability to find calls to modules and functions that do not exist. For example, if in your library code you call `ThisModuleDoesNotExist.foo(1, 2, 3)`, `mix xref unreachable` will be able to find such code and let you know about it.

Since such checks can discover possible bugs in your codebase, a new compiler called `xref` has been added to `Mix.compilers/0`, so they run by default every time you compile your code.

We have included other modes in `xref`, such as `mix xref callers Foo`, to find all places in your code that a function from the module `Foo` is called. We hope other tools and text editors can leverage such features to provide useful functionality for their users.

### Better dependency tracking

Besides `xref`, Elixir v1.3 provides better module tracking generally. For example, in previous versions, if you changed a `:path` dependency, Elixir would always fully recompile the current project. In this release, we have improved the tracking algorithms such that, if you change a `:path` dependency, only the files that depend on such dependency are recompiled.

Such improvements do not only make compilation faster but they also make working with umbrella applications much more productive. Previously, changing a sibling application triggered a full project recompilation, now Elixir can track between sibling applications and recompile only what is needed.

### mix app.tree and deps.tree

Mix also includes both `mix app.tree` and `mix deps.tree`. The first will list all applications your current project needs to start in order to boot (i.e. the ones listed in `application/0` in your `mix.exs`) while the second will lists all of your dependencies and so on recursively.

Here is a quick example from [Plug](https://github.com/elixir-lang/plug):

```elixir
$ mix app.tree
plug
├── elixir
├── crypto
├── logger
│   └── elixir
└── mime
    └── elixir
```

### mix escript.install

Mix also includes `mix escript.install` and `mix escript.uninstall` tasks for managing escripts. The tasks was designed in a way to mimic the existing `mix archive` functionality except that:

  * Archives must be used sparingly because every new archive installed affects Mix performance, as every new archive is loaded when Mix boots. Escripts solve this by being managed apart from your Elixir/Mix installed
  * Archives depends on the current Elixir version. Therefore, updating your Elixir version may break an archive. Fortunately, escripts include Elixir inside themselves, and therefore do not depend on your Elixir system version

Escripts will be installed at `~/.mix/escripts` which must be added to your [`PATH` environment variable](https://en.wikipedia.org/wiki/PATH_(variable)).

### Option parser integration

Elixir v1.3 includes improvements to the option parser, including `OptionParser.parse!/2` and `OptionParser.parse_head!/2` functions that will raise in case of invalid or unknown switches. Mix builds on top of this functionality to provide automatic error reporting solving a common complaint where invalid options were not reported by Mix tasks.

For example, invoking `mix test --unknown` in earlier Elixir versions would silently discard the `--unknown` option. Now `mix test` correctly reports such errors:

```
$ mix test --unknown
** (Mix) Could not invoke task "test": 1 error found!
--unknown : Unknown option
```

Note not all tasks have been updated to use strict option parsing. Some tasks, like `mix compile`, are actually a front-end to many other tasks, and as such, it cannot effectively assert which options are valid.

## ExUnit

ExUnit packs many improvements on the tooling side, better integration with external tools, as well as mechanisms to improve the readability of your tests.

### mix test --stale

ExUnit builds on top of `mix xref` to provide the `mix test --stale` functionality. When the `--stale` flag is given, `mix` will only run the tests that may have changed since the last time you ran `mix test --stale`. For example:

  * If you saved a test file on disk, Mix will run that file and ignore the ones that have not changed
  * If you changed a library file, for example, `lib/foo.ex` that defines `Foo`, any test that invokes a function in `Foo` directly or indirectly will also run
  * If you modify your `mix.exs` or your `test/test_helper.exs`, Mix will run the whole test suite

This feature provides a great workflow for developers, allowing them to effortlessly focus on parts of the codebase when developing new features.

### Diffing

ExUnit will now include diff-ing output every time a developer asserts `assert left == right` in their tests. For example, the assertion:

```elixir
assert "fox jumps over the lazy dog" ==
       "brown fox jumps over the dog"
```

will fail with

```elixir
  1) test compare (Difference)
     lib/ex_unit/examples/difference.exs:10
     Assertion with == failed
     lhs:  "fox jumps over the lazy dog"
     rhs:  "brown fox jumps over the dog"
     stacktrace:
       lib/ex_unit/examples/difference.exs:11: (test)
```

in a way that "lazy" in "lhs" will be shown in red to denote it has been removed from "rhs" while "brown" in "rhs" will be shown in green to denote it has been added to the "rhs".

When working with large or nested data structures, the diffing algorithm makes it fast and convenient to spot the actual differences in the asserted values.

### Test types

ExUnit v1.3 includes the ability to register different test types. This means libraries like QuickCheck can now provide functionality such as:

```elixir
defmodule StringTest do
  use ExUnit.Case, async: true
  use PropertyTestingLibrary

  property "starts_with?" do
    forall({s1, s2} <- {utf8, utf8}) do
      String.starts_with?(s1 <> s2, s1)
    end
  end
end
```

At the end of the run, ExUnit will also report it as a property, including both the amount of tests and properties:

```
1 property, 10 tests, 0 failures
```

### Named setups and describes

Finally, ExUnit v1.3 includes the ability to organize tests together in describe blocks:

```elixir
defmodule StringTest do
  use ExUnit.Case, async: true

  describe "String.capitalize/2" do
    test "uppercases the first grapheme" do
      assert "T" <> _ = String.capitalize("test")
    end

    test "lowercases the remaining graphemes" do
      assert "Test" = String.capitalize("TEST")
    end
  end
end
```

Every test inside a describe block will be tagged with the describe block name. This allows developers to run tests that belong to particular blocks, be them in the same file or across many files:

```
$ mix test --only describe:"String.capitalize/2"
```

Note describe blocks cannot be nested. Instead of relying on hierarchy for composition, we want developers to build on top of named setups. For example:

```elixir
defmodule UserManagementTest do
  use ExUnit.Case, async: true

  describe "when user is logged in and is an admin" do
    setup [:log_user_in, :set_type_to_admin]

    test ...
  end

  describe "when user is logged in and is a manager" do
    setup [:log_user_in, :set_type_to_manager]

    test ...
  end

  defp log_user_in(context) do
    # ...
  end
end
```

By restricting hierarchies in favor of named setups, it is straight-forward for the developer to glance at each describe block and know exactly the setup steps involved.

## v1.3.4 (2016-10-09)

### 1. Bug fixes

#### Elixir

  * [Kernel] Ensure the compiler does not generate unecessary variable bindings inside case statements. This improves the code emitted and make sure "unused variable warnings" are not mistakenly silenced
  * [Kernel] Move `raise` checks to runtime to avoid crashing cover on Erlang 19.1
  * [Protocol] Do not emit warnings when using protocols on opaque types

#### ExUnit

  * [ExUnit.CaptureLog] Flush Erlang's `:error_logger` before capturing to avoid mixed messages

## v1.3.3 (2016-09-17)

### 1. Enhancements

#### Elixir

  * [DateTime] Support negative integer in `DateTime.from_unix/2`
  * [Kernel.LexicalTracker] Do not consider remote typespecs as a compile-time dependency
  * [Kernel.ParallelCompiler] Do not emit deadlock messages when the process is waiting on itself
  * [Kernel.Typespec] Mark struct update syntax as generated to avoid false positives from dialyzer

#### ExUnit

  * [ExUnit] Make ExUnit server timeout configurable

#### Logger

  * [Logger] Use `:ansi_color` if one is available in metadata

#### Mix

  * [Mix] Add support for the `:sparse` option in `Mix.SCM.Git`
  * [Mix] Skip dependendency loading if `MIX_NO_DEPS` is set to `1`

### 2. Bug fixes

#### Elixir

  * [System] Use `NUL` instead of `/dev/null` on Windows when building `System.build_info`

#### IEx

  * [IEx.Autocomplete] Resolves issue with autocompletion on structs not working

#### Mix

  * [Mix] Also store external resources that are not part of the current working directory in compilation manifest
  * [Mix] Always include the compiled file source in manifests

## v1.3.2 (2016-07-15)

### 1. Enhancements

#### Elixir

  * [Kernel] Support guards in `else` clauses in `with`

#### Mix

  * [Mix] Add `MIX_NO_DEPS` env var for disabling dep loading. Used for third-party scripts and tools like Nix package manager
  * [Mix] Add `mix test --listen-on-stdin` that automatically reruns tests on stdin
  * [Mix] Disable `--warnings-as-errors` when compiling dependencies
  * [Mix] Add `--filter` option to `mix deps.unlock` to unlock only matching dependencies

### 2. Bug fixes

#### Elixir

  * [Enum] Return `nil` if enumerable halts in `Enum.find_index/3`
  * [Kernel] Do not attempt to load modules that have not been required when invoking regular functions, otherwise this invalidates the `@compile {:autoload, false}` directive.

#### Mix

  * [Mix] Ensure missing protocol dependencies are discarded in umbrella projects with shared build

#### ExUnit

  * [ExUnit.Diff] Ensure no leading or trailing when diffing some maps

## v1.3.1 (2016-06-28)

### 1. Enhancements

#### IEx

  * [IEx.Helpers] `Add import_file_if_available` for importing files only if they are available
  * [IEx.Helpers] `Add import_if_available` for importing modules only if they are available

### 2. Bug fixes

#### Elixir

  * [Kernel] Ensure structs can be expanded in dynamic module names
  * [Kernel] Ensure aliases warnings are not accidentally discarded when the same module is imported
  * [Kernel.ParallelCompiler] Ensure two modules with cyclic struct dependencies cannot run into a deadlock when compiling
  * [Kernel.Typespec] Support module attributes in remote types
  * [Module] Do not expect stacktraces to be always present when dispatching to locals during the module compilation

#### IEx

  * [IEx.Helpers] Fix `h` helper for operators

#### Mix

  * [Mix] Do not load modules for xref purposes, instead use BEAM info
  * [Mix] Ensure `deps.check` does not check archives (that's done in loadpaths)
  * [Mix] Validate application properties before traversing them
  * [Mix] Check for proper Makefile when compiling on Windows
  * [Mix] Enforce space after comma in `mix do`

## v1.3.0 (2016-06-21)

### 1. Enhancements

#### EEx

  * [EEx.Engine] Support an `init/1` function in engines that will return the initial buffer (defaults to an empty string)

#### Elixir

  * [Access] Add support for `Access.all/0`, `Access.elem/1`, `Access.key/2` and `Access.key!/1` for traversing nested data structures
  * [Calendar] Add `Calendar` and `Date`, `Time`, `NaiveDateTime` and `DateTime` types
  * [CLI] Add `--logger-otp-reports BOOL` and `--logger-sasl-reports BOOL` switches
  * [Compiler] Emit a summary of compilation errors when modules are missing
  * [Enum] Add `Enum.group_by/3` that allows developers to map on the value being grouped
  * [Enum] Make list values in maps returned by `Enum.group_by/2` and `Enum.group_by/3` preserve the order of the input enumerable instead of reversing it.
  * [Enum] Add `Enum.drop_every/2` that drops every `nth`, including the first one
  * [Exception] Suggest possible functions on `UndefinedFunctionError` for existing modules
  * [Exception] Warn if unknown fields are given to `raise/2`
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
  * [Kernel] Add sigils for date (`~D[2015-04-17]`), time (`~T[08:00:00]`) and naive date times `~N[2015-04-17 08:00:00]`
  * [Kernel] Support `@enforce_keys` on `defstruct/1` to guarantee some keys are explicitly given when building structs
  * [OptionParser] Add support for `:count` switch type
  * [OptionParser] Add `parse!/2` and `parse_head!/2` that raise `OptionParser.ParseError` in case of errors
  * [Process] Add `Process.sleep/1`
  * [Range] `Range.range?/1` now checks the validity of a range.
  * [Regex] Support `:include_captures` in `Regex.split/3`
  * [String] Add `String.myers_difference/2` for calculating the difference between two strings
  * [System] Add `System.os_time/0` and `System.os_time/1`
  * [Typespec] Add support for `%{required(foo) => bar}` and `%{optional(foo) => bar}` forms (Erlang 19 only)
  * [Typespec] Add support for `@optional_callbacks` to mark certain that certain callbacks may be optionally implemented
  * [Typespec] Introduce `%{...}` to mean any map (Erlang 19 only)
  * [URI] Add `URI.merge/2`
  * [Version] Add `Version.parse!/1`

#### ExUnit

  * [ExUnit] Show pinned variables on failed `assert ^left = right` and `assert match?(^left, right)` assertions
  * [ExUnit] Add `ExUnit.Case.register_attribute` which allow attributes to be cleaned up whenever a test is defined
  * [ExUnit] Add `ExUnit.Case.register_test` and support the ability to tag "tests" by type. This will allow projects like QuickCheck to change the wording in formatters to say "10 properties" instead of "10 tests"
  * [ExUnit] Support diffing of values when using `==` in `assert`
  * [ExUnit] Start running tests as soon as cases are loaded. This feature is enabled by default when running tests through Mix
  * [ExUnit] Raise a straight-forward error message in case a duplicate test name is defined
  * [ExUnit] Bump the default number of max cases to double of schedulers to support both IO and CPU bound tests
  * [ExUnit] Support for named setups in `setup` and `setup_all`
  * [ExUnit] Support for bundling tests together with `describe/2`

#### IEx

  * [IEx] Add `nl/2` that loads a given module on a list of nodes
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
  * [Mix] Add `mix test --stale` that uses static analysis on source files to know which tests should run when source files changes. If any test file changes, it will also re-run. Changing a configuration file or the test helper will trigger a full recompilation

### 2. Bug fixes

#### Elixir

  * [Application] Ensure `Application.spec/2` returns nil for unknown applications
  * [GenServer] Ensures `cast/2` returns `:ok` if locally registered process is not found
  * [Inspect] Ensure binaries break into new lines when inspected
  * [Kernel] Do not choke on capture operator with argument above `&191`
  * [Kernel] Raise if `defstruct` is called multiple times
  * [Kernel] Ensure `Module.create/3` respects var/alias hygiene
  * [Kernel] Support non-literal ranges on the right side of `in/2`
  * [Macro] Fix `Macro.to_string/1` on a call of a capture argument, for example `&(&1).(:x)`
  * [OptionParser] Allow `OptionParser` to parse negative numbers
  * [Record] Fix `Record.is_record/2` when dealing with non-record tuples
  * [String] Ensure `strip` also removes non-breaking whitespaces (and ensure `split` still does not split on them)
  * [URI] Use square brackets for IPv6 in `URI.to_string/1`

#### Mix

  * [Mix] Improve task not found message when Mix would include the not found task as a suggestion due to different casing
  * [Mix] Ignore lock revision when the lock is out of date when updating Mix dependencies. Before this fix, Git tags and branches in the lock file would erroneously take higher precedence than the one in `mix.exs`
  * [Mix] Only recompile empty Elixir files if they change instead of recompiling them on every run
  * [Mix] Ensure .app file is written in UTF-8 (this allows app descriptions to contain UTF-8 characters)
  * [Mix.Dep] Always specify the `:env` option internally for dependencies to avoid false positives in the dependency resolution
  * [Mix.Dep] Correctly detect conflict from cousin optional dependencies in the dependency resolution algorithm

### 3. Soft deprecations (no warnings emitted)

  * [Float] `Float.to_string/2` and `Float.to_char_list/2` has been soft-deprecated as Elixir will now attempt to print the shortest and most accurate representation by default. Developers can always fallback to `:erlang.float_to_binary/2` and `:erlang.float_to_list/2` if they need the previous functionality
  * [Kernel] `to_char_list` functions have been soft-deprecated in favor of `to_charlist`. This aligns with the naming conventions in both Erlang and Elixir
  * [String] The confusing `String.strip/2`, `String.lstrip/2` and `String.rstrip/2` API has been soft deprecated in favor of `String.trim/2`, `String.trim_leading/2` and `String.trim_trailing/2`
  * [String] The confusing `String.ljust/3` and `String.rjust/3` API has been soft deprecated in favor of `String.pad_leading/3` and `String.pad_trailing/3`
  * [Typespec] `char_list` is soft-deprecated in favor of `charlist`

### 4. Deprecations

This release deprecates many APIs that have been soft-deprecated in previous Elixir versions.

#### Elixir

  * [Dict] `Dict` is no longer a behaviour and its functions will be deprecated in upcoming releases
  * [Enum] Passing a dictionary to `Enum.group_by/3` is deprecated
  * [Kernel] `\x{H*}` in strings/sigils/charlists is deprecated
  * [Kernel] Add deprecation for `defdelegate` list arguments and `:append_first` option. The previously undocumented and deprecated support for matching has been removed
  * [Kernel] Warn if a variable is assigned inside `case`/`if`/etc and used outside the block
  * [Keyword] `Keyword.size/1` is deprecated in favor of `Kernel.length/1`
  * [Map] `Map.size/1` is deprecated in favor of `Kernel.map_size/1`
  * [Regex] The option `/r` (for ungreedy) has been deprecated in favor of `/U`
  * [Set] `Set` is no longer a behaviour and its functions will be deprecated in upcoming releases
  * [String] `String.valid_character?/1` is deprecated in favor of `String.valid?/1` with pattern matching
  * [Task] `Task.find/2` is deprecated in favor of explicit message matching
  * [URI] Passing a non-map to `URI.decode_query/2` is deprecated

## v1.2

The CHANGELOG for v1.2 releases can be found [in the v1.2 branch](https://github.com/elixir-lang/elixir/blob/v1.2/CHANGELOG.md).
