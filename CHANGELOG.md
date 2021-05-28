# Changelog for Elixir v1.13

## v1.13.0-dev

### 1. Enhancements

#### EEx

#### Elixir

  * [Code] Add `Code.string_to_quoted_with_comments/2` and `Code.quoted_to_algebra/2`
  * [Exception] Better format Elixir exceptions in Erlang
  * [Inspect] Allow default inspect fun to be set globally with `Inspect.Opts.default_inspect_fun/1`
  * [Kernel] Make `get_in` consistently abort when `nil` values are found
  * [Kernel] Improve compilation times by reducing the amount of copies of the AST across compiler processes

#### ExUnit

#### IEx

  * [IEx.Autocomplete] Add path autocompletion whenever when the cursor follows `"./` or `"/` or `"DRIVER:` where `DRIVER` is a single letter

#### Logger

  * [Logger] Add `Logger.put_application_level/2`

#### Mix

  * [mix archive.install] Run `loadconfig` before building archive
  * [mix escript.install] Run `loadconfig` before building escript
  * [mix rebar] No longer support `sub_dirs` in Rebar 2 to help migration towards Rebar 3
  * [mix test] Support `--profile-require=time` to profile the time loading test files themselves
  * [mix test] Allow filtering modules from coverage using regex

### 2. Bug fixes

#### Elixir

  * [Kernel] Raise when aliasing non-Elixir modules without `:as`
  * [Kernel] Allow `unquote_splicing` inside `%{...}` without parens
  * [Protocol] Add `defdelegate` to the list of unallowed macros inside protocols as protocols do not allow function definitions
  * [Protocol] Warn if `@callback`, `@macrocallback` and `@optional_callbacks` are defined inside protocol

#### Mix

  * [mix deps] Raise if local dep is unavailable while compiling
  * [mix local.install] Do not respect `MIX_DEPS_PATH` for install commands
  * [Mix.Shell] Add `default` option to `Mix.Shell.yes?`

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Macro] `Macro.to_string/2` is deprecated, use `Macro.to_string/1` instead
  * [System] Deprecate `System.get_pid/0`, use `System.pid/0` instead

## v1.12

The CHANGELOG for v1.12 releases can be found [in the v1.12 branch](https://github.com/elixir-lang/elixir/blob/v1.12/CHANGELOG.md).
