# Changelog for Elixir v1.13

## v1.13.0-dev

### 1. Enhancements

#### EEx

#### Elixir

  * [Code] Add `Code.string_to_quoted_with_comments/2` and `Code.quoted_to_algebra/2`
  * [Code] Add more `:token_metadata` to aliases and remote calls when parsing strings
  * [Exception] Better format Elixir exceptions in Erlang
  * [Inspect] Allow default inspect fun to be set globally with `Inspect.Opts.default_inspect_fun/1`
  * [Kernel] Make `get_in` consistently abort when `nil` values are found
  * [Kernel] Improve compilation times by reducing the amount of copies of the AST across compiler processes
  * [Kernel] Warn when `?\` is used and there is no need for a escape character
  * [Kernel] Track structs in typespecs as export deps instead of compile-time deps

#### ExUnit

#### IEx

  * [IEx.Autocomplete] Add path autocompletion whenever when the cursor follows `"./` or `"/` or `"DRIVER:` where `DRIVER` is a single letter

#### Logger

  * [Logger] Add `Logger.put_application_level/2`

#### Mix

  * [mix archive.install] Run `loadconfig` before building archive
  * [mix compile] Move Elixir version check to before deps are compiled, in order to give feedback earlier
  * [mix escript.install] Run `loadconfig` before building escript
  * [mix rebar] No longer support `sub_dirs` in Rebar 2 to help migration towards Rebar 3
  * [mix test] Support `--profile-require=time` to profile the time loading test files themselves
  * [mix test] Allow filtering modules from coverage using regex
  * [mix xref] Support multiple sinks and sources in `mix xref graph`
  * [mix xref] Add `--fail-above` option to `mix xref`
  * [mix xref] Add `--label compile-connected` to `mix xref`
  * [mix xref] Add `--label compile-direct` to `mix xref` (instead of `--only-direct`)

### 2. Bug fixes

#### Elixir

  * [Kernel] Improve error message on invalid argument for `byte_size` from binary concat
  * [Kernel] Raise when aliasing non-Elixir modules without `:as`
  * [Kernel] Allow `unquote_splicing` inside `%{...}` without parens
  * [Kernel] Ensure that waiting on a struct expansion inside a typespec is correctly tracked as waiting time in the compiler
  * [OptionParser] Validate switch types/modifiers early on to give more precise feedback
  * [Protocol] Add `defdelegate` to the list of unallowed macros inside protocols as protocols do not allow function definitions
  * [Protocol] Warn if `@callback`, `@macrocallback` and `@optional_callbacks` are defined inside protocol

#### ExUnit

  * [ExUnit] Invalidate a module's tests in `ExUnit.run/0` results if that module's `setup_all` fails
  * [ExUnit] Fix count in formatter if a module's `setup_all` fails

#### Mix

  * [mix deps] Raise if local dep is unavailable while compiling
  * [mix local.install] Do not respect `MIX_DEPS_PATH` for install commands
  * [Mix.Shell] Add `default` option to `Mix.Shell.yes?`

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Macro] `Macro.to_string/2` is deprecated, use `Macro.to_string/1` instead
  * [System] Deprecate `System.get_pid/0`, use `System.pid/0` instead

#### Mix

  * [mix escript.build] `:strip_beam` option is deprecated in favor of `:strip_beams`
  * [Mix.Config] `Mix.Config` is deprecated in favor of `Config` module

## v1.12

The CHANGELOG for v1.12 releases can be found [in the v1.12 branch](https://github.com/elixir-lang/elixir/blob/v1.12/CHANGELOG.md).
