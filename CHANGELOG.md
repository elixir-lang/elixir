# Changelog for Elixir v1.13

## v1.13.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Add `:parser_options` to EEx functions

#### Elixir

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
  * [List] Add `List.keyfind!/3`
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

#### Logger

  * [Logger] Add `Logger.put_application_level/2`

#### Mix

  * [mix archive.install] Run `loadconfig` before building archive
  * [mix compile] Move Elixir version check to before deps are compiled, in order to give feedback earlier
  * [mix deps] Add `:subdir` option to git deps
  * [mix escript.install] Run `loadconfig` before building escript
  * [mix rebar] No longer support `sub_dirs` in Rebar 2 to help migration towards Rebar 3
  * [mix test] Support `--profile-require=time` to profile the time loading test files themselves
  * [mix test] Allow filtering modules from coverage using regex
  * [mix test] Allow the exit status of ExUnit to be configured and set the default to 2
  * [mix test] Exit with a status of 3 when coverage falls below threshold
  * [mix test] Write failed manifest when suite fails due to --warnings-as-errors
  * [mix xref] Support multiple sinks and sources in `mix xref graph`
  * [mix xref] Add `--fail-above` option to `mix xref`
  * [mix xref] Add `--label compile-connected` to `mix xref`
  * [mix xref] Add `--label compile-direct` to `mix xref` (instead of `--only-direct`)

### 2. Bug fixes

#### Elixir

  * [Code] Ensure bindings with no context are returned as atoms instead of `{binding, nil}` in eval operations
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

#### Mix

  * [mix deps] Raise if local dep is unavailable while compiling
  * [mix local.install] Do not respect `MIX_DEPS_PATH` for install commands
  * [mix release] Improve release scripts to make sure shell errors cascade by avoiding exporting and defining variables at once
  * [mix release] Do not boot release if RELEASE_COOKIE is empty
  * [Mix.Shell] Add `default` option to `Mix.Shell.yes?`

### 3. Soft-deprecations (no warnings emitted)

  * [IO] `:all` on `IO.getn` is deprecated in favor of `:eof`

### 4. Hard-deprecations

#### Elixir

  * [Code] `Code.cursor_context/2` is deprecated, use `Code.Fragment.cursor_context/2` instead
  * [Macro] `Macro.to_string/2` is deprecated, use `Macro.to_string/1` instead
  * [System] `System.get_pid/0` is deprecated, use `System.pid/0` instead

#### Mix

  * [mix escript.build] `:strip_beam` option is deprecated in favor of `:strip_beams`
  * [Mix] `:exit_code` in `Mix.raise/2` has been deprecated in favor of `:exit_status`
  * [Mix.Config] `Mix.Config` is deprecated in favor of `Config` module

## v1.12

The CHANGELOG for v1.12 releases can be found [in the v1.12 branch](https://github.com/elixir-lang/elixir/blob/v1.12/CHANGELOG.md).
