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
  * [Kernel] Add `is_struct/2` guard
  * [Kernel] Support `map.field` syntax in guards
  * [Task] Add `Task.await_many/2`

#### ExUnit

  * [ExUnit.Assertion] Allow receive timeouts to be computed at runtime
  * [ExUnit.Doctest] Allow users to tag doctests with options

### 2. Bug fixes

#### Elixir

  * [Kernel] Validate values given to `:line` in quote to avoid emitting invalid ASTs

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Kernel] Deprecate remote nil arity calls without parens
  * [Supervisor] Deprecate `Supervisor.start_child/2` and `Supervisor.terminate_child/2` in favor of `DynamicSupervisor`
  * [Supervisor.Spec] Deprecate `Supervisor.Spec.worker/3` and `Supervisor.Spec.supervisor/3` in favor of the new typespecs

#### Mix

  * [mix xref] `mix xref graph --format stats` has been deprecated in favor of `mix xref stats`

## v1.10

The CHANGELOG for v1.10 releases can be found [in the v1.10 branch](https://github.com/elixir-lang/elixir/blob/v1.10/CHANGELOG.md).
