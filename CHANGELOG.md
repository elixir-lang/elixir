# Changelog for Elixir v1.13

## v1.13.0-dev

### 1. Enhancements

#### EEx

#### Elixir

  * [Inspect] Allow default inspect fun to be set globally with `Inspect.Opts.default_inspect_fun/1`
  * [Kernel] Make `get_in` consistently abort when `nil` values are found
  * [Kernel] Improve compilation times by reducing the amount of copies of the AST across compiler processes

#### ExUnit

#### IEx

  * [IEx.Autocomplete] Add path autocompletion whenever when the cursor follows `"./` or `"/` or `"DRIVER:` where `DRIVER` is a single letter

#### Logger

  * [Logger] Add `Logger.put_application_level/2`

#### Mix

  * [mix rebar] No longer support `sub_dirs` in Rebar 2 to help migration towards Rebar 3
  * [mix test] Support `--profile-require=time` to profile the time loading test files themselves

### 2. Bug fixes

#### Elixir

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

  * [System] Deprecate `System.get_pid/0`. Use `System.pid/0` instead

## v1.12

The CHANGELOG for v1.12 releases can be found [in the v1.12 branch](https://github.com/elixir-lang/elixir/blob/v1.12/CHANGELOG.md).
