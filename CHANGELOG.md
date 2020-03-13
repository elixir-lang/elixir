# Changelog for Elixir v1.11

## v1.11.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Track column information in EEx templates when enabled in the compiler
  * [EEx] Show column information in EEx error messages
  * [EEx] Support `:indentation` option when compiling EEx templates for proper column tracking

#### Elixir

  * [Calendar] Add `Calendar.stfrtime/3` for datetime formatting
  * [Code] Add `:column` to `Code.string_to_quoted*/2`
  * [Code] Add `Code.can_await_module_compilation?/0` to check if the parallel compiler is enabled and it can await for other modules to be compiled
  * [Kernel] Add `is_struct/2` guard
  * [Kernel] Support `map.field` syntax in guards
  * [Task] Add `Task.await_many/2`

#### ExUnit

  * [ExUnit.Assertion] Allow receive timeouts to be computed at runtime
  * [ExUnit.Doctest] Allow users to tag doctests with options

#### Logger

  * [Logger] Add `notice`, `critical`, `alert`, and `emergency` log levels
  * [Logger] Support structured logging by logging maps or keyword lists
  * [Logger] Allow level to be set per module with `Logger.put_module_level/2`

#### Mix

  * [mix compile.elixir] Track application boundaries in the Elixir compiler. If you invoke code from Erlang or Elixir standard libraries and you don't depend on the proper applications, a warning will be emitted. A warning will also be emitted if you invoke code from an umbrella sibling that you don't depend on - effectively forbidding cyclic dependencies between apps
  * [mix test.coverage] Add `mix test.coverage` that aggregates coverage results from umbrellas and OS partitioning

### 2. Bug fixes

#### EEx

  * [EEx] Make trimming behaviour via the `:trim` option more consistent

#### Elixir

  * [Kernel] Validate values given to `:line` in quote to avoid emitting invalid ASTs
  * [Version] Add defaults and enforce keys in `Version` struct

### 3. Soft-deprecations (no warnings emitted)

  * [Logger] `warn` log level is deprecated in favor of `warning`

### 4. Hard-deprecations

#### Elixir

  * [Supervisor] Deprecate `Supervisor.start_child/2` and `Supervisor.terminate_child/2` in favor of `DynamicSupervisor`
  * [Supervisor.Spec] Deprecate `Supervisor.Spec.worker/3` and `Supervisor.Spec.supervisor/3` in favor of the new typespecs

#### Mix

  * [mix xref] `mix xref graph --format stats` has been deprecated in favor of `mix xref stats`

## v1.10

The CHANGELOG for v1.10 releases can be found [in the v1.10 branch](https://github.com/elixir-lang/elixir/blob/v1.10/CHANGELOG.md).
