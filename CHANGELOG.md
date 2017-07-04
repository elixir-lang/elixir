# Changelog for Elixir v1.5

Elixir v1.5 brings new features, enhancements and bug fixes to Elixir. It is the first release to leverage features added as part of Erlang/OTP 20. It is also the last release that supports Erlang/OTP 18.

## UTF-8 atoms, function names and variables

Elixir v1.5 supports non-quoted atoms and variables to be in UTF-8 when using Erlang/OTP 20+. For example:

    test "こんにちは世界" do
      assert :こんにちは世界
    end

Or:

    saudação = "Bom dia!"

Elixir follows the recommendations in [Unicode Annex #31](http://unicode.org/reports/tr31/) to make the language more accessible to other languages and communities. Identifiers must still be a sequence of letters, followed by digits and combining marks. This means symbols, such as mathematical notations and emoji, are not allowed identifiers.

For a complete reference on Elixir syntax, see the [Syntax Reference](https://hexdocs.pm/elixir/syntax-reference.html). For technical details on Unicode support, see [Unicode Syntax](https://hexdocs.pm/elixir/unicode-syntax.html).

## Exception.blame

`Exception.blame/3` is a new function in Elixir that is capable of attaching debug information to certain exceptions. Currently this is used to augment `FunctionClauseError`s with a summary of all clauses and which parts of clause match and which ones didn't. For example:

    iex> Access.fetch(:foo, :bar)
    ** (FunctionClauseError) no function clause matching in Access.fetch/2

    The following arguments were given to Access.fetch/2:

        # 1
        :foo

        # 2
        :bar

    Attempted function clauses (showing 5 out of 5):

        def fetch(-%struct{} = container-, key)
        def fetch(map, key) when -is_map(map)-
        def fetch(list, key) when -is_list(list)- and is_atom(key)
        def fetch(list, key) when -is_list(list)-
        def fetch(-nil-, _key)

    (elixir) lib/access.ex:261: Access.fetch/2

In the example above, an argument that did not match or guard that did not evaluate to true are shown between `-`. If the terminal supports ANSI coloring, they are wrapped in red instead of the `-` character.

Since blaming an exception can be expensive, `Exception.blame/3` must be used exclusively in debugging situations. It is not advised to apply it to production components such as a Logger. This feature has been integrated into the compiler, the command line, ExUnit and IEx.

This feature also requires Erlang/OTP 20+.

## Streamlined child specs

Elixir v1.5 streamlines how supervisors are defined and used in Elixir. Elixir now allows child specifications, which specify how a child process is supervised, to be defined in modules. In previous versions, a project using Phoenix would write:

    import Supervisor.Spec

    children = [
      supervisor(MyApp.Repo, []),
      supervisor(MyApp.Endpoint, [])
    ]

    Supervisor.start_link(children, strategy: :one_for_one)

In Elixir v1.5, one might do:

    children = [
      MyApp.Repo,
      MyApp.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one)

The above works by calling the `child_spec/1` function on the given modules.

This new approach allows `MyApp.Repo` and `MyApp.Endpoint` to control how they run under a supervisor. This reduces the chances of mistakes being made, such as starting an Ecto repository as a worker or forgetting to declare that tasks are temporary in a supervision tree.

If it is necessary to configure any of the children, such can be done by passing a tuple instead of an atom:

    children = [
      {MyApp.Repo, url: "ecto://localhost:4567/my_dev"},
      MyApp.Endpoint
    ]

The modules `Agent`, `Registry`, `Task`, and `Task.Supervisor` have been updated to include a `child_spec/1` function, allowing them to be used directly in a supervision tree similar to the examples above. `use Agent`, `use GenServer`, `use Supervisor`, and `use Task` have also been updated to automatically define an overridable `child_spec/1` function.

Finally, child specifications are now provided as maps (data-structures) instead of the previous `Supervisor.Spec.worker/3` and `Supervisor.Spec.supervisor/3` APIs. This behaviour also aligns with how supervisors are configured in Erlang/OTP 18+. See the updated `Supervisor` docs for more information, as well as the new `Supervisor.init/2` and `Supervisor.child_spec/2` functions.

## @impl

This release also allows developers to mark which functions in a given module are an implementation of a callback. For example, when using the [Plug](https://github.com/elixir-lang/plug) project, one needs to implement both `init/1` and `call/2` when writing a Plug:

    defmodule MyApp do
      @behaviour Plug

      def init(_opts) do
        opts
      end

      def call(conn, _opts) do
        Plug.Conn.send_resp(conn, 200, "hello world")
      end
    end

The problem with the approach above is that, once more and more functions are added to the `MyApp` module, it becomes increasingly harder to know the purposes of the `init/1` and `call/2` functions. For example, for a developer unfamiliar with Plug, are those functions part of the `MyApp` API or are they implementations of a given callback?

Elixir v1.5 introduces the `@impl` attribute, which allows us to mark that certain functions are implementation of callbacks:

    defmodule MyApp do
      @behaviour Plug

      @impl true
      def init(_opts) do
        opts
      end

      @impl true
      def call(conn, _opts) do
        Plug.Conn.send_resp(conn, 200, "hello world")
      end
    end

You may even use `@impl Plug` if you want to explicitly document which behaviour defines the callback you are implementing.

Overall, using `@impl` has the following advantages:

  * Readability of the code is increased, as it is now clear which functions are part of your API and which ones are callback implementations. To reinforce this idea, `@impl true` automatically marks the function as `@doc false`, disabling documentation unless `@doc` is explicitly set

  * If you define `@impl` before a function that is not a callback, Elixir will error. This is useful in case of typos or in case the behaviour definition changes (such as a new major version of a library you depend on is released)

  * If you use `@impl` in one implementation, Elixir will force you to declare `@impl` for all other implementations in the same module, keeping your modules consistent

## Calendar improvements

This release brings further improvements to Calendar types. It adds arithmetic and others functions to `Time`, `Date`, `NaiveDateTime` and `Datetime` as well as conversion between different calendars.

## v1.5.0-rc.1 (2017-07-04)

### 1. Enhancements

#### Elixir

  * [Base] Optimise Base encode/decode
  * [Calendar] Implement Inspect for DateTime with Calendar.ISO
  * [Kernel] Cache the AST on definitions. This speeds up the compilation time from 10% to 15% measured across different projects.

### 2. Bug fixes

#### Elixir

  * [Calendar] Return `{:error, :invalid_time}` for wrong precision instead of crashing
  * [Enumerable] Raise `Protocol.UndefinedError` on bad functions in Enumerable implementation
  * [Inspect] Do not use colors when inspecting for error messages
  * [Kernel] Improve error message on invalid patterns and guards
  * [Kernel] Do not warn false positives about unused variables on rescue (regression)
  * [Stream] Fix stream cycle over empty enumerable
  * [StringIO] Fix encoding and performance issues in `StringIO.get_until`

#### ExUnit

  * [ExUnit] Load ExUnit configuration as late as possible (regression)

## v1.5.0-rc.0 (2017-06-25)

### 1. Enhancements

#### Elixir

  * [Access] Optimize Access.get/2
  * [Calendar] Limit `Calendar.ISO` up to year 10000
  * [Calendar] Add Rata Die format for conversions between Calendars and `Date.convert/2`, `Time.convert/2`, `NaiveDateTime.convert/2` and `DateTime.convert/2` (as well as bang variants)
  * [Calendar] Add `:calendar` field to `Time` struct
  * [Calendar] Add `Time.diff/3`, `Date.add/2`, `Date.diff/2`, `DateTime.diff/3`
  * [Calendar] Add `Date.range/2`
  * [Calendar] Add `Date.new/4`, `DateTime.utc_now/1`, `NaiveDateTime.new/8` and `Time.new/5` that allow specifing calendar
  * [Enum] Add `Enum.chunk_by/4` and `Stream.chunk_by/4`
  * [Exception] Add `Exception.blame/3` that adds metadata to exceptions
  * [File] Add `File.read_link/1` and `File.read_link!/1`
  * [File] Introduce `:trim_bom` option for `File.stream!/2`
  * [Inspect] Add `:printable_limit` to control the limit of printable structures
  * [Integer] Add `Integer.gcd/2`
  * [Kernel] Use the new `debug_info` chunk in OTP 20. This provides a mechanism for tools to retrieve the Elixir AST from beam files
  * [Kernel] `defoverridable/1` accepts a module name as argument and marks all callbacks as overridable
  * [Kernel] Allow non-quoted Unicode atoms and variables according to Unicode Annex #31 (see Unicode Syntax document)
  * [Kernel] Warn when a `:__struct__` key is used when building/updating structs
  * [Keyword] Add `replace/3` and `replace!/3` for replacing an existing key
  * [List] `List.starts_with?/2`
  * [Macro] Introduce `Macro.generate_arguments/2`
  * [Map] Optimize `Map.merge/3` by choosing merging direction
  * [Map] Add `replace/3` and `replace!/3` for replacing an existing key
  * [Map] Raise `BadMapError` in `Map.equal?/2` when either of the two arguments is not a map
  * [MapSet] Reduce `MapSet` size when serialized to approximately half
  * [Process] Add `Process.cancel_timer/2`
  * [Protocol] Show available implementations on `Protocol.UndefinedError` if the protocol has been consolidated
  * [Registry] Support ETS guard conditions in `Registry.match/3`
  * [Registry] Support `parallel: true` in `Registry.dispatch/3`
  * [Supervisor] Add `Supervisor.init/2` and `Supervisor.child_spec/2`
  * [Supervisor] Allow `module` and `{module, arg}` to be given to `Supervisor.start_link/2` and invoke `module.child_spec(arg)` on each argument
  * [Task] Support `:on_timeout` in `Task.async_stream` to control how tasks are terminated
  * [Task] Add `ordered: false` support to `Task.async_stream`

#### ExUnit

  * [ExUnit] Show code snippet from test source file in case of test errors
  * [ExUnit] Use `Exception.blame/3` when formatting test errors
  * [ExUnit] Make `assert_raise/2` fail if the underlying exception has a broken `message/1` implementation
  * [ExUnit] Add `start_supervised/2` and `stop_supervised/1` to ExUnit. Processes started by this function are automatically shut down when the test exits

#### IEx

  * [IEx.Autocomplete] Support autocompletion of variable names
  * [IEx.Autocomplete] Support autocompletion of functions imported using `import Mod, only: [...]`
  * [IEx.Evaluator] Use `Exception.blame/3` when showing errors in the terminal
  * [IEx.Helpers] Add `e/1` IEx helper to list all exports in a module
  * [IEx.Info] Implement `IEx.Info` protocol for calendar types

#### Logger

  * [Logger] Add `metadata: :all` configuration to log all metadata

#### Mix

  * [mix compile.elixir] Add `--all-warnings` option to Elixir compiler that shows all warnings from the previous compilation (instead of just of the files being compiled)
  * [mix escript.build] Strip debug information from escripts by default and add option `:strip_beam` which defaults to true
  * [mix loadpaths] Ensure `--no-deps-check` do not trigger SCM callbacks (such as `git`)
  * [mix local.hex] Add `--if-missing` flag to `local.hex` mix task
  * [mix profile.cprof] Add `Mix.Tasks.Profile.Cprof` for count-based profiling
  * [mix new] New styling for generated applications

### 2. Bug fixes

#### Elixir

  * [File] Support `:ram`/`:raw` files in `File.copy/2`
  * [Kernel] Support guards on anonymous functions of zero arity
  * [Kernel] Fix compilation of maps used as maps keys inside matches
  * [Module] `on_definition/6` callback receives body wrapped in a keyword list, such as `[do: body]`. This solves a bug where it was impossible to distinguish between a bodyless clause and a function that returns `nil`.
  * [Record] Properly escape quoted expressions passed to `defrecord`
  * [Regex] Fix `inspect/2` for regexes with `/` terminator in them
  * [String] Consider Unicode non-characters valid according to the specification in `String.valid?/1`

#### ExUnit

  * [ExUnit] Properly account failed tests when `setup_all` fails

#### IEx

  * [IEx] Skip autocompletion of module names that are invalid without being quoted
  * [IEx] Skip autocompletion of functions with default arguments with `@doc false`
  * [IEx] Do not start oldshell alongside IEx

#### Mix

  * [mix compile.elixir] Store multiple sources in case of module conflicts. This solves an issue where `_build` would get corrupted when compiling Elixir projects with module conflicts
  * [mix compile.erlang] Do not silently discard Erlang compile errors
  * [mix compile.protocols] Ensure protocol implementations do not "disappear" when switching between applications in umbrella projects by having separate consolidation paths per project

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Kernel] `not left in right` is soft-deprecated in favor of `left not in right`

### 4. Deprecations

#### Elixir

  * `Atom.to_char_list/1`, `Float.to_char_list/1`, `Integer.to_char_list/1`, `Integer.to_char_list/2`, `Kernel.to_char_list/1`, `List.Chars.to_char_list/1`, `String.to_char_list/1` have been deprecated in favor of their `to_charlist` version. This aligns with the naming conventions in both Erlang and Elixir
  * [Enum] Deprecate `Enum.filter_map/3` in favor of `Enum.filter/2` + `Enum.map/2` or for-comprehensions
  * [GenEvent] Deprecate `GenEvent` and provide alternatives in its docs
  * [Kernel] Using `()` to mean `nil` is deprecated
  * [Kernel] `:as_char_lists value` in `Inspect.Opts.t/0` type, in favor of `:as_charlists`
  * [Kernel] `:char_lists` key in `Inspect.Opts.t/0` type, in favor of `:charlists`
  * [Module] Using Erlang parse transforms via `@compile {:parse_transform, _}` is deprecated
  * [Stream] Deprecate `Stream.filter_map/3` in favor of `Stream.filter/2` + `Stream.map/2`
  * [String] `String.ljust/3` and `String.rjust/3` are deprecated in favor of `String.pad_leading/3` and `String.pad_trailing/3` with a binary padding
  * [String] `String.strip/1` and `String.strip/2` are deprecated in favor of `String.trim/1` and `String.trim/2`
  * [String] `String.lstrip/1` and `String.rstrip/1` are deprecated in favor of `String.trim_leading/1` and `String.trim_trailing/1`
  * [String] `String.lstrip/2` and `String.rstrip/2` are deprecated in favor of `String.trim_leading/2` and `String.trim_trailing/2` with a binary as second argument
  * [Typespec] `char_list/0` type is deprecated in favor of `charlist/0`

#### EEx

  * [EEx] Deprecate `<%= ` in middle and end expressions, e.g.: `<%= else %>` and `<%= end %>`

## v1.4

The CHANGELOG for v1.4 releases can be found [in the v1.4 branch](https://github.com/elixir-lang/elixir/blob/v1.4/CHANGELOG.md).
