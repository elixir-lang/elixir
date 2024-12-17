# Changelog for Elixir v1.19

## v1.19.0-dev

### 1. Enhancements

#### Elixir

  * [Code] Add `:migrate_call_parens_on_pipe` formatter option
  * [Kernel] Add `min/2` and `max/2` as valid guards

#### IEx

  * [IEx.Autocomplete] Functions annotated with `@doc group: "Name"` metadata will appear in disctint groups during autocompletion

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Node] `Node.start/2-3` is deprecated in favor of `Node.start/2` with a keyword list

### 4. Hard deprecations

#### Elixir

  * [Code] The `on_undefined_variable: :warn` is deprecated. Relying on undefined variables becoming function calls will not be supported in the future
  * [File] Passing a callback as third argument to `File.cp/3` is deprecated, pass it as a `on_conflict: callback` option instead
  * [File] Passing a callback as third argument to `File.cp_r/3` is deprecated, pass it as a `on_conflict: callback` option instead
  * [Kernel.ParallelCompiler] Passing `return_diagnostics: true` as an option is required on `compile`, `compile_to_path` and `require`

#### Logger

  * [Logger] The `:backends` configuration is deprecated, either set the `:default_handler` to false or start backends in your application start callback

#### Mix

  * [mix] The `:default_task`, `:preferred_cli_env`, and `:preferred_cli_target` configuration inside `def project` in your `mix.exs` has been deprecated in favor of `:default_task`, `:preferred_envs` and `:preferred_targets` inside the `def cli` function
  * [mix do] Using commas as task separator in `mix do` (such as `mix do foo, bar`) is deprecated, use `+` instead (as in `mix do foo + bar`)

## v1.18

The CHANGELOG for v1.18 releases can be found [in the v1.18 branch](https://github.com/elixir-lang/elixir/blob/v1.18/CHANGELOG.md).
