# Changelog for Elixir v1.13

Elixir v1.13 requires Erlang/OTP 22+.

## v1.13.0-dev

### 1. Enhancements

#### EEx

#### Elixir

  * [Inspect] Allow default inspect fun to be set globally with `Inspect.Opts.default_inspect_fun/1`
  * [Kernel] Make `get_in` consistently abort when `nil` values are found

#### ExUnit

#### IEx

#### Logger

  * [Logger] Add `Logger.put_application_level/2`

#### Mix

### 2. Bug fixes

#### Elixir

  * [Protocol] Add `defdelegate` to the list of unallowed macros inside protocols as protocols do not allow function definitions

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [System] Deprecate `System.get_pid/0`. Use `System.pid/0` instead

## v1.12

The CHANGELOG for v1.12 releases can be found [in the v1.12 branch](https://github.com/elixir-lang/elixir/blob/v1.12/CHANGELOG.md).
