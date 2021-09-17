# Changelog for Elixir v1.13

The focus behind Elixir v1.13 has been on tooling, mainly tooling related to code formatting, code fragments, code reflection, and code recompilation. A lot of this functionality will directly impact developers working on large codebases and provide meaningful quality of life improvements for those working on Elixir tooling and environments, such as IDEs, notebooks, etc.

## Semantic recompilation

Elixir v1.13 comes with many improvements to the compiler, so it recompiles your files less frequently. In particular:

  * The digest of the files are considered in addition to their size. This avoids recompiling many files when switching or rebasing branches.

  * Changing your `mix.exs` will no longer trigger a full recompilation, unless you specifically change the configurations used by the Elixir compiler (`:elixirc_paths` and `:elixirc_options`).

  * Changing compile-time configuration files (`config/config.exs` and any other file imported from it) now only recompiles the project files that depend on the reconfigured applications, instead of a full recompilation. However, if you change the configuration of your application itself, the whole project is still recompiled.

  * Adding or updating a dependency now only recompiles the project files that depend on the modified a dependency. Removing a dependency still triggers a whole project recompilation.

  * If your project has both Erlang and Elixir files, changing an Erlang file will now recompile only the Elixir files that depend on it.

In a nutshell, Elixir went from triggering full recompilations whenever any of `mix.exs`, `config/config.exs`, `src/*`, and `mix.lock` changed on disk to semantic recompilations where it only fully recompiles when:

  * you change the compilation options in `mix.exs`
  * you change the configuration for the current project in `config/config.exs`
  * you remove a dependency

## mix xref

`mix xref` is a tool that analyzes relationships between files. By analyzing the compile-time and runtime dependencies between files, it allows developers to understand what files have to be recompiled whenever a file changes.

Elixir v1.13 comes with many improvements to `mix xref`, such as:

  * `mix xref graph` now supports `--label` to be set to "compile-direct" and "compile-connected", which returns the direct compile-time dependencies ("compile-direct") or just those that generate additional transitive dependencies ("compile-connected")

  * A new `mix xref trace FILE` subcommand receives a file and returns all dependencies in said file, including the line and what caused said dependency (a function/macro call, an alias, a struct, etc).

  * All `mix xref` subcommands support the `--fail-above` flag, which allows you to enforce your project has at most a certain number of compile-time cycles, transitive compile-time dependencies, etc.

  * `mix xref graph` now supports multiple `--sink` and `--source` to be given.

With these improvements, it has become simpler to understand the impact code recompilation has in our codebases and how to limit it.

## Code fragments

The `Code` module also got a companion module called `Code.Fragment`, which hosts functions that work on incomplete code, as is often the scenario in editors, command line, etc. The module contains different heuristics to analyze the source code and return context informational.

Thanks to these improvements, `IEx`' autocomplete got several quality of life improvements, such as the autocompletion of sigils, structs, and paths. For example, typing `~<TAB>` now shows:

```iex
iex(1)> ~
~C (sigil_C)    ~D (sigil_D)    ~N (sigil_N)    ~R (sigil_R)
~S (sigil_S)    ~T (sigil_T)    ~U (sigil_U)    ~W (sigil_W)
~c (sigil_c)    ~r (sigil_r)    ~s (sigil_s)    ~w (sigil_w)

```

Adding the sigil and pressing tab then shows the available operators:

```iex
iex(1)> ~r
"      """    '      '''    (      /      <      [      {      |

```

Similarly, `%<TAB>` now shows only the available structs (exceptions excluded), instead of all modules:

```elixir
iex(1)> %File.St
File.Stat      File.Stream
```

Finally, new compilation tracers have been added, alongside a handful of functions in `Module` to retrieve module metadata, which can be used to enrich suggestions in programming environments.

## Extended code formatting

The `Code` has been augmented with two functions: `Code.string_to_quoted_with_comments/2` and `Code.quoted_to_algebra/2`. Those functions allow someone to retrieve the Elixir AST with their original source code comments, and then convert this AST to formatted code. In other words, those functions provide a wrapper around the Elixir Code Formatter, supporting developers who wish to create tools that directly manipulate source code.

## v1.13.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Add `:parser_options` to EEx functions

#### Elixir

  * [CLI] Support `--short-version` on the CLI that does not boot the VM
  * [Code] Add `Code.string_to_quoted_with_comments/2` and `Code.quoted_to_algebra/2`
  * [Code] Add more `:token_metadata` to aliases and remote calls when parsing strings
  * [Code] Add `Code.Fragment` module to provide best-effort information from code fragments. The module currently provides an updated `Code.Fragment.cursor_context/2` with operator support and `Code.Fragment.surround_context/2` which looks at a given position in a fragment and find its surrounding delimiters
  * [Code] Add `{:on_module, bytecode, :none}` trace to compilation tracers
  * [Enum] Optimize `Enum.concat/1` for lists of lists
  * [Exception] Better format Elixir exceptions in Erlang
  * [Inspect] Allow default inspect fun to be set globally with `Inspect.Opts.default_inspect_fun/1`
  * [IO] Allow `:eof` to be given as limit to `IO.getn/2`
  * [Kernel] Make `get_in` consistently abort when `nil` values are found
  * [Kernel] Improve compilation times by reducing the amount of copies of the AST across compiler processes
  * [Kernel] Warn when `?\` is used and there is no need for a escape character
  * [Kernel] Track structs in typespecs as export deps instead of compile-time deps
  * [Keyword] Add `Keyword.validate/2`
  * [List] Add `List.keyfind!/3`
  * [Macro.Env] Add the following reflection functions: `required?/2`, `lookup_import/2`, `fetch_alias/2`, and `fetch_macro_alias/2`
  * [Module] Support `:nillify_clauses` in `Module.get_definition/3`
  * [Module] Add `Module.attributes_in/1` and `Module.overridables_in/1`
  * [OptionParser] Add "did you mean?" suggestions to `OptionParser.ParseError` messages
  * [Record] Add record reflection via `@__records__`
  * [Task] Add `Task.completed/1`

#### ExUnit

  * [ExUnit.CaptureIO] Add `with_io/3` to return result with captured io
  * [ExUnit.CaptureLog] Add `with_log/2` to return result with captured logs

#### IEx

  * [IEx.Autocomplete] Add path autocompletion whenever when the cursor follows `"./` or `"/` or `"DRIVER:` where `DRIVER` is a single letter
  * [IEx.Autocomplete] Add autocompletion for sigils and structs

#### Logger

  * [Logger] Add `Logger.put_application_level/2`

#### Mix

  * [mix archive.install] Run `loadconfig` before building archive
  * [mix compile] Move Elixir version check to before deps are compiled, in order to give feedback earlier
  * [mix compile.elixir] Do not recompile files if their modification time change but their contents are still the same and the .beam files are still on disk
  * [mix compile.elixir] Do not recompile all Elixir sources when Erlang modules change, only dependent ones
  * [mix compile.elixir] Do not recompile Elixir files if `mix.exs` changes, instead recompile only files using `Mix.Project` or trigger a recompilation if a compiler option changes
  * [mix compile.elixir] Only recompile needed files when a dependency is added or updated
  * [mix compile.elixir] Only recompile needed files when a dependency is configured
  * [mix deps] Add `:subdir` option to git deps
  * [mix escript.install] Run `loadconfig` before building escript
  * [mix local.rebar] No longer support `sub_dirs` in Rebar 2 to help migration towards Rebar 3
  * [mix local.rebar] Support `--if-missing` option when installing Rebar
  * [mix local.rebar] Set `REBAR_PROFILE=prod` when compiling Rebar dependencies
  * [mix test] Support `--profile-require=time` to profile the time loading test files themselves
  * [mix test] Allow filtering modules from coverage using regex
  * [mix test] Allow the exit status of ExUnit to be configured and set the default to 2
  * [mix test] Exit with a status of 3 when coverage falls below threshold
  * [mix test] Write failed manifest when suite fails due to --warnings-as-errors
  * [mix xref] Support multiple sinks and sources in `mix xref graph`
  * [mix xref] Add `trace` subcommand to print compilation dependencies between files
  * [mix xref] Add `--fail-above` option to `mix xref`
  * [mix xref] Add `--label compile-connected` to `mix xref`
  * [mix xref] Add `--label compile-direct` to `mix xref` (instead of `--only-direct`)

### 2. Bug fixes

#### Elixir

  * [Application] Warn if `Application.compile_env` or `Application.compile_env!` are called without a require
  * [Code] Ensure bindings with no context are returned as atoms instead of `{binding, nil}` in eval operations
  * [Kernel] Raise if `__CALLER__` or `__ENV__` or `__STACKTRACE__` are used in match
  * [Kernel] Improve error message on invalid argument for `byte_size` from binary concat
  * [Kernel] Raise when aliasing non-Elixir modules without `:as`
  * [Kernel] Allow `unquote_splicing` inside `%{...}` without parens
  * [Kernel] Ensure that waiting on a struct expansion inside a typespec is correctly tracked as waiting time in the compiler
  * [Kernel] Correctly parse the atom `.` as a keyword list key
  * [Kernel] Do not leak variables from the first generator in `with` and `for` special forms
  * [OptionParser] Validate switch types/modifiers early on to give more precise feedback
  * [Protocol] Add `defdelegate` to the list of unallowed macros inside protocols as protocols do not allow function definitions
  * [Protocol] Warn if `@callback`, `@macrocallback` and `@optional_callbacks` are defined inside protocol
  * [URI] Only percent decode if followed by hex digits (according to https://url.spec.whatwg.org/#percent-decode)

#### ExUnit

  * [ExUnit] Invalidate a module's tests in `ExUnit.run/0` results if that module's `setup_all` fails
  * [ExUnit] Fix count in formatter if a module's `setup_all` fails

#### Logger

  * [Logger] Raise clear error message for invalid `:compile_time_purge_matching` configuration

#### Mix

  * [mix deps] Raise if local dep is unavailable while compiling
  * [mix local.install] Do not respect `MIX_DEPS_PATH` for install commands
  * [mix release] Improve release scripts to make sure shell errors cascade by avoiding exporting and defining variables at once
  * [mix release] Do not boot release if RELEASE_COOKIE is empty
  * [Mix.Shell] Add `default` option to `Mix.Shell.yes?`

### 3. Soft-deprecations (no warnings emitted)

#### Elixir

  * [IO] `:all` on `IO.getn` is deprecated in favor of `:eof`
  * [Code] Environment options in `Code.eval_quoted/3` and `Code.eval_string/3`, such as `:aliases` and `:tracers`, have been deprecated in favor of passing an environment

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
