# Changelog for Elixir v1.6

## Code formatter

The big feature in Elixir v1.6 is the addition of a code formatter and an accompanying `mix format` task that adds automatic formatting to your projects.

The goal of the formatter is to automate the styling of codebases into a unique and consistent layout used across teams and the whole community. Code is now easier to write, as you no longer need to concern yourself with formatting rules. Code is also easier to read, as you no longer need to convert the styles of other developers in your mind.

The formatter also helps new developers to learn the language, by giving immediate feedback on code structure, and eases code reviews by allowing teams to focus on business rules and code quality, rather than code style.

To automatically format your codebase, you can run the new `mix format` task. A `.formatter.exs` file may be added to your project root for rudimentary formatter configuration. The mix task also supports flags for CI integration. For instance, you can make your build or a Pull Request fail if the code is not formatted accordingly. We also recommend developers to check their favorite editor and see if they already provide key bindings for `mix format`, allowing a file or a code snippet to be formatted without ceremony.

The Elixir codebase itself has been already fully formatted and all further contributions are expected to contain formatted code. We recommend existing codebases to be formatted in steps. While the formatter will correctly handle long lines and complex expressions, refactoring the code by breaking those into variables or smaller functions as you format them will lead to overall cleaner and more readable codebases.

## Dynamic Supervisor

Supervisors in Elixir are responsible for starting, shutting down and restarting child process when things go wrong. Most of the interaction with supervisors happen with the Supervisor module and it contains three main strategies: `:one_for_one`, `:rest_for_one` and `:one_for_all`.

However, sometimes the children of a supervisor are not known upfront and are rather started dynamically. For example, if you are building a web server, you have each request being handled by a separate supervised process. Those cases were handled in the Supervisor module under a special strategy called `:simple_one_for_one`.

Unfortunately, this special strategy changed the semantics of the supervisor in regards to initialization and shutdown. Plus some APIs expected different inputs or would be completely unavailable depending on the supervision strategy.

Elixir v1.6 addresses this issue by introducing a new `DynamicSupervisor` module, which encapsulates the old `:simple_one_for_one` strategy and APIs in a proper module while allowing the documentation and API of the `Supervisor` module to focus on its main use cases. Having a separate `DynamicSupervisor` module also makes it simpler to add new features to the dynamic supervisor, such as the new `:max_children` option that limits the maximum number of children supervised dynamically.

## `@deprecated` and `@since` attributes

This release also introduces two new attributes associated to function definitions: `@deprecated` and `@since`. The former marks if a function or macro is deprecated, the latter annotates the version the API was introduced:

    @doc "Breaks a collection into chunks"
    @since "1.0.0"
    @deprecated "Use chunk_every/2 instead"
    def chunk(collection, chunk_size) do
      chunk_every(collection, chunk_size)
    end

The `mix xref` task was also updated to warn if your project calls deprecated code. So if a definition is marked as `@deprecated` and a module invokes it, a warning will be emitted during compilation. This effectively provides libraries and frameworks a mechanism to deprecate code without causing multiple warnings to be printed in runtime and without impacting performance.

Note those attributes are not yet available to tools that generate documentation. Such functionality will be added in future releases as it requires changes to how Elixir stores documentation in BEAM files. We still recommend developers to properly annotate their APIs, as the information will then be already available when the tooling is updated.

## defguard and defguardp

Elixir provides the concepts of guards: expressions used alongside pattern matching to select a matching clause. Let's see an example straight from Elixir's home page:

    def serve_drinks(%User{age: age}) when age >= 21 do
      # Code that serves drinks!
    end

`%User{age: age}` is matching on a `User` struct with an age field and `when age >= 21` is the guard.

Since only a handful of constructs are [allowed in guards](https://hexdocs.pm/elixir/guards.html#content), if you were in a situation where you had to check the age to be more than or equal to 21 in multiple times, extracting the guard to a separate function would be [less than obvious and error prone](https://github.com/elixir-lang/elixir/issues/2469). To address those issues, this release introduces `defguard/1` and `defguardp/1`:

    defguard is_drinking_age(age) when age >= 21

    def serve_drinks(%User{age: age}) when is_drinking_age(age) do
      # Code that serves drinks!
    end

## IEx improvements

IEx also got its share of improvements. The new code formatter allows us to pretty print code snippets, types and specifications, improving the overall experience when exploring code through the terminal.

The autocomplete mechanism also got smarter, being able to provide context autocompletion. For example, typing `t Enum.` and hitting TAB will autocomplete only the types in Enum (in contrast to all functions). Typing `b GenServer.` and hitting TAB will autocomplete only the behaviour callbacks.

Finally, the breakpoint functionality added in Elixir v1.5 has been improved to support pattern matching and guards. For example, to pattern match on a function call when the first argument is the atom `:foo`, you may do:

    break! SomeFunction.call(:foo, _, _)

## mix xref

`mix xref` is a task added in Elixir v1.3 which provides general information about how modules and files in an application depend on each other. This release brings many improvements to `xref`, extending the reach of the analysis and helping developers digest the vast amount of data it produces.

One of such additions is the `--include-siblings` option that can be given to all `xref` commands inside umbrella projects. For example, to find all of the callers of a given module or function in an umbrella:

    $ mix xref callers SomeModule --include-siblings

The `graph` command in `mix xref` now can also output general statistics about the graph. In [the hexpm project](https://github.com/hexpm/hexpm), you would get:

    $ mix xref graph --format stats
    Tracked files: 129 (nodes)
    Compile dependencies: 256 (edges)
    Structs dependencies: 46 (edges)
    Runtime dependencies: 266 (edges)

    Top 10 files with most outgoing dependencies:
      * test/support/factory.ex (18)
      * lib/hexpm/accounts/user.ex (13)
      * lib/hexpm/accounts/audit_log.ex (12)
      * lib/hexpm/web/controllers/dashboard_controller.ex (12)
      * lib/hexpm/repository/package.ex (12)
      * lib/hexpm/repository/releases.ex (11)
      * lib/hexpm/repository/release.ex (10)
      * lib/hexpm/web/controllers/package_controller.ex (10)
      * lib/mix/tasks/hexpm.stats.ex (9)
      * lib/hexpm/repository/registry_builder.ex (9)

    Top 10 files with most incoming dependencies:
      * lib/hexpm/web/web.ex (84)
      * lib/hexpm/web/router.ex (29)
      * lib/hexpm/web/controllers/controller_helpers.ex (29)
      * lib/hexpm/web/controllers/auth_helpers.ex (28)
      * lib/hexpm/web/views/view_helpers.ex (27)
      * lib/hexpm/web/views/icons.ex (27)
      * lib/hexpm/web/endpoint.ex (23)
      * lib/hexpm/ecto/changeset.ex (22)
      * lib/hexpm/accounts/user.ex (19)
      * lib/hexpm/repo.ex (19)

`mix xref graph` also got the `--only-nodes` and `--label` options. The former asks Mix to only output file names (nodes) without the edges. The latter allows you to focus on certain relationships:

      # To get all files that depend on lib/foo.ex
      mix xref graph --sink lib/foo.ex --only-nodes

      # To get all files that depend on lib/foo.ex at compile time
      mix xref graph --label compile --sink lib/foo.ex --only-nodes

      # To get all files lib/foo.ex depends on
      mix xref graph --source lib/foo.ex --only-nodes

      # To limit statistics only to compile time dependencies
      mix xref graph --format stats --label compile

Those improvements will help developers better understand the relationship between files and reveal potentially complex parts of their systems.

Other improvements in Mix include better compiler diagnostics for editor integration, support for the `--slowest N` flag in `mix test` that shows the slowest tests in your suite, and a new `mix profile.eprof` task that provides time based profiling, complementing the existing `mix profile.cprof` (count based) and `mix profile.fprof` (flame based).

## v1.6.6 (2018-06-20)

This release supports Erlang/OTP 21.0 by removing all warnings and by properly supporting the new Erlang logger module.

### 1. Bug fixes

#### Elixir

  * [Base] Do not raise when finding bad digits in `Base.decode32!` with `case: :mixed`
  * [Code] Preserve the user's choice when `fn` is followed by a newline and it has only a single clause
  * [DynamicSupervisor] Properly account for restarting children in the `:max_children` configuration
  * [String] Add performant impl for string upcase/downcase `:ascii` mode
  * [Task.Supervisor] Fix type spec for `start_child/4`

#### Logger

  * [Logger] Do not crash truncation when truncate is set to infinity

#### Mix

  * [mix format] Match files starting with dot

## v1.6.5 (2018-05-07)

This release supports Erlang/OTP 21.0-rc by removing all warnings and by properly redirecting logger output. Note it is not guaranteed it will support Erlang/OTP 21.0 final.

### 1. Bug fixes

#### Elixir

  * [Code] Preserve the user's choice in the formatter on parens call with next break fits
  * [Code] Preserve the user's choice in the formatter on calls without parens when we have one argument per line
  * [Code] Fix formatting when there is a tilde in the first element of a bitstring
  * [Kernel] Support specsdiff flag on `__info__` spec clauses
  * [Kernel] Do not exclude hygienic vars in `defguard`
  * [Kernel.SpecialForms] Mark `for` comprehensions as generated to avoid dialyzer warnings
  * [Macro] Make sure `Macro.to_string/2` emits valid quoted expressions
  * [Task] Support `:infinity` timeout on `Task.yield_many/2`
  * [Task.Supervisor] Do not crash spawning supervised tasks when the parent process is dead
  * [URI] Fix parsing of URIs with trailing `?`

## v1.6.4 (2018-03-16)

### 1. Bug fixes

#### Elixir

  * [Code.Formatter] Do not double escape quoted keyword list identifiers
  * [Kernel] Properly support `into: binary` in Erlang/OTP 20.3

## v1.6.3 (2018-03-09)

### 1. Enhancements

#### Elixir

  * [Code.Formatter] Support comments in the middle of pipelines, `when` and `|` expressions

### 2. Bug fixes

#### Elixir

  * [Code.Formatter] Consider commas when breaking groups
  * [Code.Formatter] Ensure proper precedence between `&` and operators
  * [Code.Formatter] Consider `.formatter.exs` when formatting stdin

#### Logger

  * [Logger.Translator] Ensure logger doesn't crash when reporting named `DynamicSupervisor`

## v1.6.2 (2018-02-28)

### 1. Enhancements

#### Mix

  * [mix compile.erlang] Teach Mix erlang compiler alternative spelling for `-behavior` declaration
  * [mix format] Support the `:subdirectories` configuration that points to other directories with their own `.formatter.exs` file. This is useful in umbrella applications. `mix new --umbrella` has also been changed to use this new configuration by default
  * [mix format] Include the current environment for missing dependency errors

### 2. Bug fixes

#### Elixir

  * [Code.Formatter] Ensure `->` does not exceed line length
  * [DynamicSupervisor] Properly tag error reports generated by dynamic supervisors so they can be properly translated by `Logger`
  * [DynamicSupervisor] Consider extra arguments during child restart
  * [Kernel] Ensure arguments given to a guard defined with `defguard` are evaluated in the correct order
  * [Module] Do not remove docs for previous function declaration when `@impl true` is used
  * [Supervisor] Ensure `use Supervisor` properly adds the `@behaviour Supervisor` annotation

#### Mix

  * [Mix.Shell] Bring back `Mix.Shell.cmd/2` - this arity was defined via a default argument that was accidentally removed

## v1.6.1 (2018-01-29)

### 1. Enhancements

#### Elixir

  * [DynamicSupervisor] Implement `child_spec/1` for DynamicSupervisor
  * [Kernel] Raise better error messages on invalid map syntax

### 2. Bug fixes

#### Elixir

  * [Code.Formatter] Only rearrange `not in` operator if explicitly opted-in
  * [Code.Formatter] Ensure `do` blocks do not exceed line length on calls with a single argument
  * [Collectable] Support bitstrings in Collectable and for-comprehensions (regression in v1.6.0)
  * [GenServer] Do not override user own `@opts` attribute
  * [Enum] Reintroduce zipping of any enumerable of enumerables in `Enum.zip/1` (regression in v1.6.0)
  * [Macro] Reorder kw blocks in `Macro.to_string/1` to avoid warnings
  * [Protocol] Fix protocol consolidation when some chunks may be missing
  * [Stream] Reintroduce zipping of any enumerable of enumerables in `Stream.zip/1` (regression in v1.6.0)
  * [Supervisor] Do not override user own `@opts` attribute
  * [Supervisor] Add `@spec` to second clause of `start_link/2`

#### ExUnit

  * [ExUnit.Case] Reintroduce `:case` in ExUnit setup/setup_all/test context

## v1.6.0 (2018-01-17)

### 1. Enhancements

#### EEx

  * [EEx] Allow markers `/` and `|` to be used in a custom EEx engine

#### Elixir

  * [Calendar] Add truncate to `Time`, `DateTime` and `NaiveDateTime` to facilitate microsecond precision pruning
  * [Code] Add `format_string!/2` and `format_file!/2` for automatic code formatting
  * [Code] Support column annotations in quoted expressions with `columns: true` in `Code.string_to_quoted/2`
  * [DynamicSupervisor] Add `DynamicSupervisor` designed to manage children that are added and removed dynamically
  * [Exception] Make `Exception.blame/3` extensible by adding an optional `blame/2` callback to exceptions
  * [Exception] Improve the printing of guards on blamed exceptions
  * [Enumerable] Add `Enumerable.slice/1` and optimize many `Enum` operations with the new protocol. This allows data-structures with index-based random access to provide a non-linear implementation
  * [Inspect] Show UTF-8 BOM on inspected strings
  * [Inspect.Algebra] Add `:strict` and `:flex` breaks - this gives more control over the document fitting
  * [Inspect.Algebra] Allow a group to inherit the parent group break
  * [Inspect.Algebra] Add `force_unfit/1` and `next_break_fits/2` which give more control over document fitting
  * [Inspect.Algebra] Add `collapse_lines/1` for collapsing multiple lines to a maximum value
  * [Inspect.Algebra] Allow `nest/2` to be `:reset` or be set to the current `:cursor` position
  * [Kernel] Prefix variables with V when emitting Erlang code. This improves the integration with tools such as Erlang code formatters and the GUI debugger
  * [Kernel] Warn on the use of `length(x) == 0` in guards
  * [Kernel] Warn if `catch` comes before `rescue` in try
  * [Kernel] Warn if heredoc is outdented compared to its closing quotes
  * [Kernel] Add `defguard/1` and `defguardp/1` to make it easier to build guard-safe macros
  * [Kernel.ParallelCompiler] Add `compile/2`, `compile_to_path/3` and `require/2` which provide detailed information about warnings and errors
  * [Kernel.SpecialForms] Support the `uniq: true` flag in `for` comprehensions
  * [Module] Introduce `@deprecated` and `@since` attributes
  * [Module] Emit conflicting behaviour warnings if the same behaviour is given more than once
  * [List] Rearrange equals and inserts for shorter diff scripts in `List.myers_difference/2`
  * [Record] Allow `:macros` and `:includes` to be given to `Record.extract/2`
  * [Stream] Add `Stream.intersperse/2`
  * [String] Update to Unicode 10
  * [String] Allow passing empty string `match` to `String.replace/4`
  * [String] Support context and language sensitive operations in `String.upcase/2` and `String.downcase/2`. Currently only the `:greek` context is supported
  * [String] Support `:ascii` conversion in `String.upcase/2` and `String.downcase/2`
  * [Time] Add `Time.add/3`

#### ExUnit

  * [ExUnit.Assertions] Perform inclusive checks in `assert_in_delta`
  * [ExUnit.Callbacks] Add `ExUnit.Callbacks.start_supervised!/2`
  * [ExUnit.Case] Generate a random seed per test based on the test suite seed

#### IEx

  * [IEx.Autocomplete] Provide contextual autocompletion: `t Enum.` will autocomplete types, `b Enum` will autocomplete callbacks
  * [IEx.CLI] Provide hints for developers when a bad host name is given to `--remsh`
  * [IEx.Helpers] Automatically include specs when showing documentation for functions/macros
  * [IEx.Helpers] Improve formatting of behaviours and typespecs by using the formatter
  * [IEx.Helpers] Allow pattern matching and guard expressions when on `IEx.break!`

#### Logger

  * [Logger] Add `:discard_threshold` to Logger to help with message queue overflow

#### Mix

  * [mix app.start] Add `--preload-modules` to `mix app.start`
  * [mix archive.build] Allow `mix archive.build` to bundle dot files via an option
  * [mix compile] Define a behavior for Mix compiler tasks and return diagnostics from compiler tasks
  * [mix compile] Track struct dependencies between files and recompile them only if the struct changes
  * [mix deps] Support `:system_env` option when specifying dependencies
  * [mix format] Add a `mix format` task that formats the given files (or the files specified in a `.formatter.exs` file)
  * [mix profile.eprof] Add a new task for time-based profiling with eprof
  * [mix test] Run all functions in a describe block by giving the `file:line` the describe block starts
  * [mix test] Report the top N slowest tests with the `--slowest N` flag
  * [mix test] Report the number of doctests and tests separately
  * [mix xref] Support `--include-siblings` in reports for umbrella support
  * [mix xref] Add `mix xref graph --format stats`
  * [mix xref] Add `--only-nodes` and `--label` filters to mix xref graph
  * [mix xref] Add `mix xref deprecated` that shows the callsite of deprecated functions

### 2. Bug fixes

#### Elixir

  * [CLI] Support path with spaces as argument to elixir.bat
  * [Inspect] Properly handle minus signal for non-decimal negative integers
  * [Integer] Do not raise on non-integer values in `is_odd`/`is_even`
  * [Kernel] Solve a precedence issue between `&` and `|`, such as `[&Foo.bar/1 | &Baz.bat/2]`
  * [Kernel] Do not load dynamic Elixir modules as `:in_memory` as this value is not officially supported by the code server. Instead, use an empty list, which is the same value used by Erlang.
  * [Kernel] Validate variable struct name is atom when used in pattern matching
  * [Kernel] No longer generate documentation for `defdelegate` functions automatically to avoid overriding previously specified `@doc`
  * [Macro] Fix `Macro.to_string/2` for tuple calls, such as `alias Foo.{Bar, Baz}`
  * [MapSet] Return valid MapSet when unioning a legacy MapSet
  * [Regex] Return a leading empty space when splitting on empty pattern. This makes the `split` operation consistent with the other operations in the `Regex` module
  * [Stream] Ensure `Stream.chunk_while/4` does not emit more elements than necessary when halted
  * [String] Return a leading empty space when splitting on empty string. This makes the `split` operation consistent with the other operations in the `String` module
  * [URI] Preserve empty fragments in `URI.parse/1`

#### Mix

  * [mix app.start] Improve the quality of reports if app fails to boot
  * [mix cmd] Allow `mix cmd` to be invoked multiple times without marking it as executed
  * [mix deps] Ensure optional dependencies in umbrella applications are loaded
  * [mix deps.update] Ensure transitive new non-Hex dependencies are also fetched when a repo is updated
  * [mix xref] Take compile dependencies with higher priority than runtime ones when building a graph
  * [mix xref] Handle external files for xref callers and warnings

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [GenServer] Warn if `init/1` is not defined in `GenServer`. This brings GenServer closer to the implementation in OTP and aligns all behaviours to require the `init/1` callback
  * [Inspect.Algebra] `surround/3` and `surround_many/6` are deprecated in favor of `container_doc/6`
  * [Kernel] Specifying map types with variable keys without defining the type as required/optional is deprecated
  * [Kernel.ParallelCompiler] `files/2` and `files_to_path/3` are deprecated in favor of `compile/2` and `compile_to_path/3`
  * [Kernel.ParallelRequire] `files/2` is deprecated in favor of `Kernel.ParallelCompiler.require/2`
  * [Supervisor] The `:simple_one_for_one` strategy is deprecated in favor of `DynamicSupervisor`
  * [Supervisor] Passing a list of args to `Supervisor.start_child/2` is deprecated in favor of `DynamicSupervisor`
  * [Task.Supervisor] Passing `:restart` and `:shutdown` to `Task.Supervisor.start_link/2` is deprecated (it should be passed on start child instead)

#### ExUnit

  * [ExUnit.Formatter] `:case_started` and `:case_finished` events are deprecated in favor of `:module_started` and `:module_finished`

#### Mix

  * [Mix.Compilers.Erlang] Returning `{:ok, val} | :error` from custom Erlang compilers is deprecated in favor of `{:ok, val, warnings} | {:error, errors, warnings}`

### 4. Deprecations

#### Elixir

  * [Enum] `Enum.partition/2` is deprecated in favor of `Enum.split_with/2`
  * [Keyword] `Keyword.replace/3` is deprecated in favor of `Keyword.fetch/2` and `Keyword.put/3`
  * [Map] `Map.replace/3` is deprecated in favor of `Map.fetch/2` and `Map.put/3`
  * [Macro] `Macro.unescape_tokens/1` and `Macro.unescape_tokens/2` are deprecated in favor of `Enum.map/2`
  * [Range] Deprecate `Range.range?/1` in favor of pattern matching on `_ .. _`

## v1.5

The CHANGELOG for v1.5 releases can be found [in the v1.5 branch](https://github.com/elixir-lang/elixir/blob/v1.5/CHANGELOG.md).
