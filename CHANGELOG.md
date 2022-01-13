# Changelog for Elixir v1.13

The focus behind Elixir v1.13 has been on tooling, mainly tooling related to code formatting, code fragments, code reflection, and code recompilation. A lot of this functionality will directly impact developers working on large codebases and provide meaningful quality of life improvements for those working on Elixir tooling and environments, such as IDEs, notebooks, etc.

## Semantic recompilation

Elixir v1.13 comes with many improvements to the compiler, so it recompiles your files less frequently. In particular:

  * The digest of the files are considered in addition to their size. This avoids recompiling many files when switching or rebasing branches.

  * Changing your `mix.exs` will no longer trigger a full recompilation, unless you specifically change the configurations used by the Elixir compiler (`:elixirc_paths` and `:elixirc_options`).

  * Changing compile-time configuration files (`config/config.exs` and any other file imported from it) now only recompiles the project files that depend on the reconfigured applications, instead of a full recompilation. However, if you change the configuration of your application itself, the whole project is still recompiled.

  * Adding, updating or removing a dependency now only recompiles the project files that depend on the modified a dependency.

  * If your project has both Erlang and Elixir files, changing an Erlang file will now recompile only the Elixir files that depend on it.

In a nutshell, Elixir went from triggering full recompilations whenever any of `mix.exs`, `config/config.exs`, `src/*`, and `mix.lock` changed on disk to semantic recompilations. Now it only fully recompiles when:

  * you change the compilation options in `mix.exs`
  * you change the configuration for the current project in `config/config.exs`

## mix xref

`mix xref` is a tool that analyzes relationships between files. By analyzing the compile-time and runtime dependencies between files, it allows developers to understand what files have to be recompiled whenever a file changes.

Elixir v1.13 comes with many improvements to `mix xref`, such as:

  * `mix xref graph` now supports `--label` to be set to "compile-connected", which returns all compile-time dependencies that lead to additional transitive dependencies.

  * A new `mix xref trace FILE` subcommand receives a file and returns all dependencies in said file, including the line and what caused said dependency (a function/macro call, an alias, a struct, etc).

  * All `mix xref` subcommands support the `--fail-above` flag, which allows you to enforce your project has at most a certain number of compile-time cycles, transitive compile-time dependencies, etc.

  * `mix xref graph` now supports multiple `--sink` and `--source` to be given.

With these improvements, it has become simpler to understand the impact code recompilation has in our codebases and how to limit it.

## Code fragments

The `Code` module got a companion module called `Code.Fragment`, which hosts functions that work on incomplete code, as is often the scenario in editors, interactive shells, etc. The module contains different heuristics to analyze the source code and return context informational.

Thanks to these improvements, `IEx`' autocomplete got several quality of life improvements, such as the autocompletion of sigils, structs, and paths. For example, typing `~<TAB>` now shows:

```iex
iex(1)> ~
~C (sigil_C)    ~D (sigil_D)    ~N (sigil_N)    ~R (sigil_R)
~S (sigil_S)    ~T (sigil_T)    ~U (sigil_U)    ~W (sigil_W)
~c (sigil_c)    ~r (sigil_r)    ~s (sigil_s)    ~w (sigil_w)

```

Adding the sigil letter and pressing tab then shows the available delimiters:

```iex
iex(1)> ~r
"      """    '      '''    (      /      <      [      {      |

```

Similarly, `%<TAB>` now shows only the available structs (exceptions excluded), instead of all modules:

```elixir
iex(1)> %File.St
File.Stat      File.Stream
```

Once you define the struct, you can hit `tab` to show all struct fields available:

```elixir
iex(1)> %URI{
authority:    fragment:     host:         path:         port:
query:        scheme:       userinfo:
```

As you fill a field in, the already filled fields no longer show up:

```elixir
iex(1)> %URI{path: "/example",
authority:    fragment:     host:         port:         query:
scheme:       userinfo:
```

Along the same lines, `SyntaxError` and `TokenMissingError` were improved to show a code snippet whenever possible:

```elixir
$ elixir -e "hello + * world"
** (SyntaxError) nofile:1:9: syntax error before: '*'
    |
  1 | hello + * world
    |         ^
```

Finally, new compilation tracers have been added, alongside a handful of functions in `Module` to retrieve module metadata, which can be used to enrich suggestions in programming environments.

## Extended code formatting

The `mix format` task has been augmented with the notion of plugins. Plugins can teach the formatter how to format new files and how to format sigils, via the `Mix.Tasks.Format` behaviour.

For example, imagine that your project uses Markdown in two distinct ways: via a custom `~M` sigil and via files with the `.md` and `.markdown` extensions. A custom plugin would look like this:

```elixir
defmodule MixMarkdownFormatter do
  @behaviour Mix.Tasks.Format

  def features(_opts) do
    [sigils: [:M], extensions: [".md", ".markdown"]]
  end

  def format(contents, opts) do
    # logic that formats markdown
  end
end
```

Now any application can use your formatter as follows:

```elixir
# .formatter.exs
[
  # Define the desired plugins
  plugins: [MixMarkdownFormatter],
  # Remember to update the inputs list to include the new extensions
  inputs: ["{mix,.formatter}.exs", "{config,lib,test}/**/*.{ex,exs}", "posts/*.{md,markdown}"]
]
```

Finally, the `Code` module has also been augmented with two functions: `Code.string_to_quoted_with_comments/2` and `Code.quoted_to_algebra/2`. Those functions allow someone to retrieve the Elixir AST with their original source code comments, and then convert this AST to formatted code. In other words, those functions provide a wrapper around the Elixir Code Formatter, supporting developers who wish to create tools that directly manipulate and custom format Elixir source code.

## v1.13.2 (2022-01-13)

### 1. Enhancements

#### Mix

  * [mix format] Allow plugins to also format `.ex` and `.exs` files
  * [mix release] Allow bypassing application mode validation in release spec
  * [mix test] Print a message when the suite fails due to the coverage threshold

### 2. Bug fixes

#### Elixir

  * [Code] Do not emit warnings on `Code.Fragment.container_cursor_to_quoted/2`
  * [Kernel] Fix a crash when a for-comprehension with `:uniq` was used inside another comprehension with `:uniq`
  * [Kernel] Ensure `env.context_modules` is properly set inside optimized `defmodule`
  * [Keyword] Deprecate the recently added `Keyword.map/2` as it is equivalent to `Keyword.new/2`
  * [Map] Deprecate the recently added `Map.map/2` as it is equivalent to `Map.new/2`
  * [Protocol] Warn on zero arity callbacks inside protocols

## v1.13.1 (2021-12-14)

### 1. Bug fixes

#### Elixir

  * [Code] Do not show code snippets in `SyntaxError` and `TokenMissingError` if line is empty
  * [Exception] Do not fail blaming `ArgumentError` for improper lists on `apply/3`
  * [Macro] Set a max `line_length` for `Macro.to_string/1`
  * [Macro] Fix formatting of lists on module attributes for `Macro.to_string/1`
  * [String] Fix incorrect codepoint byte counting in `slice` with negative positions in ranges
  * [Task] Ensure async streams can be consumed from another process than the one that creates them
  * [URI] Undeprecate `URI.parse/1` as `URI.new/1` is too strict in many common cases
  * [URI] Make sure `URI.new/1` returns nil for empty paths

#### IEx

  * [IEx] Make sure the `--version` flag halts IEx

#### Mix

  * [Mix] Make protocol consolidation part of the `Mix.install/2` cache

## v1.13.0 (2021-12-03)

### 1. Enhancements

#### EEx

  * [EEx] Add `:parser_options` to EEx functions

#### Elixir

  * [Calendar] Add `c:Calendar.year_of_era/3` to support calendars where the beginning of a new era does not align with the beginning of a new year
  * [CLI] Support `--short-version` on the CLI that does not boot the VM
  * [Code] Add `Code.string_to_quoted_with_comments/2` and `Code.quoted_to_algebra/2`
  * [Code] Add more `:token_metadata` to aliases and remote calls when parsing strings
  * [Code] Add `Code.Fragment` module to provide best-effort information from code fragments. The module currently provides an updated `Code.Fragment.cursor_context/2` with operator support and `Code.Fragment.surround_context/2` which looks at a given position in a fragment and find its surrounding delimiters
  * [Code] Allow custom sigil formatting on `Code.format_string!/2`
  * [Code] Add `{:on_module, bytecode, :none}` trace to compilation tracers
  * [Enum] Optimize `Enum.concat/1` for lists of lists
  * [Enum] Add `Enum.slide/3`
  * [Exception] Better format Elixir exceptions in Erlang
  * [Inspect] Allow default inspect fun to be set globally with `Inspect.Opts.default_inspect_fun/1`
  * [IO] Allow `:eof` to be given as limit to `IO.getn/2`
  * [Kernel] Support the `:sigils` option in `import Mod, only: :sigils` and allow the sigil modifiers to be also digits
  * [Kernel] Make `get_in` consistently abort and return `nil` when `nil` values are found (previously Elixir would raise an error in this case). This allows a user to use `get_in` as a safe navigation operator.
  * [Kernel] Improve compilation times by reducing the amount of copies of the AST across compiler processes
  * [Kernel] Raise if trying to define a module with a slash in its name
  * [Kernel] Warn when `?\` is used and there is no need for a escape character
  * [Kernel] Track structs in typespecs as export deps instead of compile-time deps
  * [Kernel] Add power operator (`**/2`)
  * [Keyword] Add `Keyword.validate/2`
  * [Keyword] Implement `Keyword.filter/2` and `Keyword.map/2`
  * [List] Add `List.keyfind!/3`
  * [Macro] Add `Macro.prewalker/1` and `Macro.postwalker/1`
  * [Macro.Env] Add the following reflection functions: `required?/2`, `lookup_import/2`, `fetch_alias/2`, and `fetch_macro_alias/2`
  * [Map] Implement `Map.filter/2` and `Map.map/2`
  * [Module] Support `:nillify_clauses` in `Module.get_definition/3`
  * [Module] Add `Module.attributes_in/1` and `Module.overridables_in/1`
  * [OptionParser] Add "did you mean?" suggestions to `OptionParser.ParseError` messages
  * [Record] Add record reflection via `@__records__`
  * [Task] Add `Task.completed/1`
  * [Task] Add `Task.ignore/1` to keep a task running but ignoring all of its results
  * [Task] Reduce the amount of copying `Task.async*` functions
  * [URI] Add `URI.new/1` and `URI.new!/1`

#### ExUnit

  * [ExUnit] Show hint if comparing different but equivalent strings
  * [ExUnit.CaptureIO] Add `with_io/3` to return result with captured io
  * [ExUnit.CaptureLog] Add `with_log/2` to return result with captured logs

#### IEx

  * [IEx.Autocomplete] Add path autocompletion whenever when the cursor follows `"./` or `"/` or `"DRIVER:` where `DRIVER` is a single letter
  * [IEx.Autocomplete] Add autocompletion for sigils, struct names, and struct fields
  * [IEx.Helpers] Allow multiple modules to be given to `r/1`

#### Logger

  * [Logger] Add `Logger.put_application_level/2`
  * [Logger] Print all log levels in accordance to Erlang/OTP. This also means `[warn]` is now shown as `[warning]`

#### Mix

  * [Mix] Add `MIX_INSTALL_FORCE` environment variable support
  * [Mix] Support `:config` and `:system_env` in `Mix.install/2`
  * [Mix] Add `Mix.installed?/0`
  * [Mix.Shell] Add `:default` option to `Mix.Shell.yes?`
  * [mix archive.install] Run `loadconfig` before building archive
  * [mix compile] Move Elixir version check to before deps are compiled, in order to give feedback earlier
  * [mix compile.elixir] Do not recompile files if their modification time change but their contents are still the same and the .beam files are still on disk
  * [mix compile.elixir] Do not recompile all Elixir sources when Erlang modules change, only dependent ones
  * [mix compile.elixir] Do not recompile Elixir files if `mix.exs` changes, instead recompile only files using `Mix.Project` or trigger a recompilation if a compiler option changes
  * [mix compile.elixir] Only recompile needed files when a dependency is added, updated or removed
  * [mix compile.elixir] Only recompile needed files when a dependency is configured
  * [mix deps] Add `:subdir` option to git deps
  * [mix escript.install] Run `loadconfig` before building escript
  * [mix format] Support `:plugins` in `mix format` that can hook into custom extensions and sigils
  * [mix format] Add `Mix.Tasks.Format.formatter_for_file/2`
  * [mix local.rebar] No longer support `sub_dirs` in Rebar 2 to help migration towards Rebar 3
  * [mix local.rebar] Support `--if-missing` option when installing Rebar
  * [mix local.rebar] Set `REBAR_PROFILE=prod` when compiling Rebar dependencies
  * [mix test] Support `--profile-require=time` to profile the time loading test files themselves
  * [mix test] Allow filtering modules from coverage using regex
  * [mix test] Allow the exit status of ExUnit to be configured and set the default to 2
  * [mix test] Exit with a status of 3 when coverage falls below threshold
  * [mix test] Write failed manifest when suite fails due to --warnings-as-errors
  * [mix test] Ignore `MIX_TEST_PARTITION` when partitions set to 1
  * [mix xref] Support multiple sinks and sources in `mix xref graph`
  * [mix xref] Add `trace` subcommand to print compilation dependencies between files
  * [mix xref] Add `--fail-above` option to `mix xref`
  * [mix xref] Add `--label compile-connected` to `mix xref`

### 2. Bug fixes

#### EEx

  * [EEx] Accept comments in EEx between do and the first clause
  * [EEx] Accept EEx expressions where `->` is followed by newline

#### Elixir

  * [Application] Allow any expression as first argument of `compile_env`
  * [Application] Warn if `Application.compile_env` or `Application.compile_env!` are called without a require
  * [Code] Make sure `:static_atoms_encoder` in `Code.string_to_quoted/2` also applies to quoted keyword keys
  * [Code] Ensure bindings with no context are returned as atoms instead of `{binding, nil}` in eval operations
  * [Inspect] Fix a bug when inspecting a non-binary bitstring with colors
  * [Kernel] Reject bidirectional formatting characters in strings and comments
  * [Kernel] Support escaping of terminators in uppercase sigils heredocs for consistency
  * [Kernel] Raise if `__CALLER__` or `__ENV__` or `__STACKTRACE__` are used in match
  * [Kernel] Improve error message on invalid argument for `byte_size` from binary concat
  * [Kernel] Raise when aliasing non-Elixir modules without `:as`
  * [Kernel] Allow `unquote_splicing` inside `%{...}` without parens
  * [Kernel] Ensure that waiting on a struct expansion inside a typespec is correctly tracked as waiting time in the compiler
  * [Kernel] Correctly parse the atom `.` as a keyword list key
  * [Kernel] Do not leak variables from the first generator in `with` and `for` special forms
  * [Kernel] Fix column number on strings with NFD characters
  * [Kernel] Fix a bug where a combination of dynamic line in `quote` with `unquote` of remote calls would emit invalid AST metadata
  * [OptionParser] Validate switch types/modifiers early on to give more precise feedback
  * [Protocol] Add `defdelegate` to the list of unallowed macros inside protocols as protocols do not allow function definitions
  * [Protocol] Warn if `@callback`, `@macrocallback` and `@optional_callbacks` are defined inside protocol
  * [Protocol] Ensure protocol metadata is deterministic on consolidation
  * [Range] Always show step when range is descending
  * [String] Update Unicode database to version 14.0
  * [URI] Only percent decode if followed by hex digits (according to https://url.spec.whatwg.org/#percent-decode)
  * [Version] Ensure proper precedence of `and`/`or` in version requirements

#### ExUnit

  * [ExUnit] Fix formatter and counters from `ExUnit.run/0` to consider all tests in a module whenever if a module's `setup_all` fails
  * [ExUnit] Allow doctests newlines to be terminated by CRLF

#### IEx

  * [IEx] Fix the loss of `.iex.exs` context after a pry session
  * [IEx] Stop evaluator before exiting IEx server to avoid evaluators leaking

#### Logger

  * [Logger] Raise clear error message for invalid `:compile_time_purge_matching` configuration
  * [Logger] Fix a bug where Logger would not reset its discard counter under some scenarios

#### Mix

  * [mix compile.elixir] Track transitive runtime dependencies coming from local/path dependencies
  * [mix compile.elixir] Recompile file if `@external_resource` is deleted
  * [mix compile.elixir] Print number of compiling files on all compiler cycles. This will make the `Compiling N files (.ex)` show up multiple times if necessary
  * [mix deps] Raise if local dep is unavailable while compiling
  * [mix deps.unlock] Fix blank output when unlocking a dependency that is not locked
  * [mix local.install] Do not respect `MIX_DEPS_PATH` for install commands
  * [mix release] Improve release scripts by making sure shell errors cascade (this is done by avoiding exporting and defining variables in a single step)
  * [mix release] Do not boot release if `RELEASE_COOKIE` is empty
  * [mix release] Allow releases running as a daemon to be restarted
  * [mix release] Raise proper error message when non-serializable values are in configs
  * [mix test] Fix coverage engine to also tag `case`, `cond`, and `receive` branches where the right side is a literal

### 3. Soft-deprecations (no warnings emitted)

#### Elixir

  * [Code] Environment options in `Code.eval_quoted/3` and `Code.eval_string/3`, such as `:aliases` and `:tracers`, have been deprecated in favor of passing an environment
  * [IO] `:all` on `IO.getn` is deprecated in favor of `:eof`
  * [URI] `URI.parse/1` is deprecated in favor of `URI.new/1` and `URI.new!/1`

#### Mix

  * [mix format] `Mix.Tasks.Format.formatter_opts_for_file/2` is deprecated in favor of `Mix.Tasks.Format.formatter_for_file/2`

### 4. Hard-deprecations

#### Elixir

  * [Code] `Code.cursor_context/2` is deprecated, use `Code.Fragment.cursor_context/2` instead
  * [Macro] `Macro.to_string/2` is deprecated, use `Macro.to_string/1` instead
  * [System] `System.get_pid/0` is deprecated, use `System.pid/0` instead
  * [Version] Using `!` or `!=` in version requirements is deprecated, use `~>` or `>=` instead

#### Mix

  * [mix escript.build] `:strip_beam` option is deprecated in favor of `:strip_beams`
  * [Mix] `:exit_code` in `Mix.raise/2` has been deprecated in favor of `:exit_status`
  * [Mix.Config] `Mix.Config` is deprecated in favor of `Config` module

## v1.12

The CHANGELOG for v1.12 releases can be found [in the v1.12 branch](https://github.com/elixir-lang/elixir/blob/v1.12/CHANGELOG.md).
