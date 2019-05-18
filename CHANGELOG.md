# Changelog for Elixir v1.9

## v1.9.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Allow more complex mixed expressions when tokenizing

#### Elixir

  * [Access] Allow `Access.at/1` to handle negative index
  * [CLI] Add support for `--boot`, `--boot-var`, `--erl-config`, `--pipe-to`, `--rpc-eval`, and `--vm-args` options
  * [Code] Add `static_atom_encoder` option to `Code.string_to_quoted/2`
  * [Code] Support `:force_do_end_blocks` on `Code.format_string!/2` and `Code.format_file!/2`
  * [Code] Do not raise on deadlocks on `Code.ensure_compiled/1`
  * [Config] Add `Config`, `Config.Reader` and `Config.Provider` modules for working with configuration
  * [File] Add `File.rename!/2`
  * [Inspect] Add `:inspect_fun` to `Inspect.Opts`
  * [Kernel] Add `~U` sigil for UTC date times
  * [Kernel] Optimize `&super/arity` and `&super(&1)`
  * [Kernel] Optimize generated code for `with` with a catch-all clause
  * [Kernel] Validate `__struct__` key in map returned by `__struct__/0,1`
  * [Module] Add `Module.get_attribute/3`
  * [Protocol] Improve `Protocol.UndefinedError` messages to also include the type that was attempted to dispatch on
  * [Protocol] Optimize performance of dynamic dispatching for non-consolidated protocols
  * [Record] Include field names in generated type for records
  * [Registry] Add `Registry.select/2`
  * [System] Add `System.restart/0`, `System.pid/0` and `System.no_halt/1`
  * [System] Add `System.get_env/2`, `System.fetch_env/1`, and `System.fetch_env!/1`
  * [System] Support `SOURCE_DATE_EPOCH` for reproducible builds

#### ExUnit

  * [ExUnit] Allow multiple `:exclude` on configuration/CLI
  * [ExUnit.DocTest] No longer wrap doctest errors in custom exceptions. They ended-up hiding more information than showing
  * [ExUnit.DocTest] Display the actual doctest code when doctset fails

#### IEx

  * [IEx.CLI] Copy ticktime from remote node on IEx --remsh
  * [IEx.CLI] Automatically add a host on node given to --remsh

#### Logger

  * [Logger] Use a decentralized mode computation for Logger which allows overloads to be detected more quickly
  * [Logger] Use `persistent_term` to store configuration whenever available for performance

#### Mix

  * [Mix] Follow XDG base dir specification in Mix for temporary and configuration files
  * [Mix.Generator] Add `copy_file/3`, `copy_template/4`, and `overwite?/2`
  * [mix archive.uninstall] Allow `mix archive.uninstall APP` to uninstall any installed version of APP
  * [mix new] No longer generate a `config/` directory for mix new
  * [mix release] Add support for releases
  * [mix release.init] Add templates for release configuration
  * [mix test] Allow running tests for a given umbrella app from the umbrella root with `mix test apps/APP/test`. Test failures also include the `apps/APP` prefix in the test location

### 2. Bug fixes

#### EEx

  * [EEx] Consistently trim newlines when you have a single EEx expression per line on multiple lines

#### Elixir

  * [Code] Quote `::` in `Code.format_string!/1` to avoid ambiguity
  * [Enum] Ensure the first equal entry is returned by `Enum.min/2` and `Enum.max/2`
  * [Kernel] Improve error message when string interpolation is used in a guard
  * [Kernel] Properly merge and handle docs for callbacks with multiple clauses
  * [Kernel] Guarantee reproducible builds on modules with dozens of specs
  * [Kernel] Resolve `__MODULE__` accordingly in nested `defmodule` to avoid double nesting
  * [Kernel] Type variables starting with an underscore (`_foo`) should not raise compile error
  * [Kernel] Keep order of elements when macro `in/2` is expanded with a literal list on the right-hand side
  * [Kernel] Print proper location on undefined function error from dynamically generated functions
  * [System] Make sure `:init.get_status/0` is set to `{:started, :started}` once the system starts
  * [Protocol] Ensure `debug_info` is kept in protocols
  * [Regex] Ensure inspect returns valid `~r//` expressions when they are manually compiled with backslashes
  * [Registry] Fix ETS leak in `Registry.register/2` for already registered calls in unique registries while the process is still alive

#### ExUnit

  * [ExUnit] Raise error if attempting to run single line tests on multiple files

#### IEx

  * [IEx] Automatically shut down IEx if we receive EOF

#### Mix

  * [mix compile] Ensure Erlang-based Mix compilers (erlang, leex, yecc) set valid position on diagnostics
  * [mix compile] Ensure compilation halts in an umbrella project if one of the siblings fail to compile
  * [mix test] Do not consider modules that are no longer cover compiled when computing coverage report, which could lead to flawed reports

### 3. Soft-deprecations (no warnings emitted)

#### Mix

  * [Mix.Config] `Mix.Config` has been deprecated in favor of the `Config` module that now ships as part of Elixir itself. Reading configuration files should now be done by the `Config.Reader` module

### 4. Hard-deprecations

#### Elixir

  * [CLI] Deprecate `--detached` option, use `--erl "-detached"` instead
  * [Map] Deprecate Enumerable keys in `Map.drop/2`, `Map.split/2`, and `Map.take/2`

#### Mix

  * [Mix.Project] Deprecate `Mix.Project.load_paths/1` in favor of `Mix.Project.compile_path/1`

## v1.8

The CHANGELOG for v1.8 releases can be found [in the v1.8 branch](https://github.com/elixir-lang/elixir/blob/v1.8/CHANGELOG.md).
