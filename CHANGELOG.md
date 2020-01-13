# Changelog for Elixir v1.11

## v1.11.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Track column information in EEx templates

#### Elixir

  * [Code] Add `:column` to `Code.string_to_quoted*/2`
  * [Kernel] Add `is_struct/2` guard
  * [Kernel] Support `map.field` syntax in guards

### 2. Bug fixes

### 3. Soft-deprecations (no warnings emitted)

### 4. Hard-deprecations

#### Elixir

  * [Supervisor] Deprecate `Supervisor.start_child/2` and `Supervisor.terminate_child/2` in favor of `DynamicSupervisor`
  * [Supervisor.Spec] Deprecate `Supervisor.Spec.worker/3` and `Supervisor.Spec.supervisor/3` in favor of the new typespecs

#### Mix

  * [mix xref] `mix xref graph --format stats` has been deprecated in favor of `mix xref stats`

## v1.10

The CHANGELOG for v1.10 releases can be found [in the v1.10 branch](https://github.com/elixir-lang/elixir/blob/v1.10/CHANGELOG.md).
