# Changelog for Elixir v1.9

## Releases

The main feature in Elixir v1.9 is the addition of releases. A release is a self-contained directory that consists of your application code, all of its dependencies, plus the whole Erlang Virtual Machine (VM) and runtime. Once a release is assembled, it can be packaged and deployed to a target as long as the target runs on the same operating system (OS) distribution and version as the machine running the `mix release` command.

You can start a new project and assemble a release for it in three easy steps:

    $ mix new my_app
    $ cd my_app
    $ MIX_ENV=prod mix release

A release will be assembled in `_build/prod/rel/my_app`. Inside the release, there will be a `bin/my_app` file which is the entry point to your system. It supports multiple commands, such as:

  * `bin/my_app start`, `bin/my_app start_iex`, `bin/my_app restart`, and `bin/my_app stop` - for general management of the release

  * `bin/my_app rpc COMMAND` and `bin/my_app remote` - for running commands on the running system or to connect to the running system

  * `bin/my_app eval COMMAND` - to start a fresh system that runs a single command and then shuts down

  * `bin/my_app daemon` and `bin/my_app daemon_iex` - to start the system as a daemon on Unix-like systems

  * `bin/my_app install` - to install the system as a service on Windows machines

### Why releases?

Releases allow developers to precompile and package all of their code and the runtime into a single unit. The benefits of releases are:

  * Code preloading. The VM has two mechanisms for loading code: interactive and embedded. By default, it runs in the interactive mode which dynamically loads modules when they are used for the first time. The first time your application calls `Enum.map/2`, the VM will find the `Enum` module and load it. There’s a downside. When you start a new server in production, it may need to load many other modules, causing the first requests to have an unusual spike in response time. Releases run in embedded mode, which loads all available modules upfront, guaranteeing your system is ready to handle requests after booting.

  * Configuration and customization. Releases give developers fine grained control over system configuration and the VM flags used to start the system.

  * Self-contained. A release does not require the source code to be included in your production artifacts. All of the code is precompiled and packaged. Releases do not even require Erlang or Elixir in your servers, as they include the Erlang VM and its runtime by default. Furthermore, both Erlang and Elixir standard libraries are stripped to bring only the parts you are actually using.

  * Multiple releases. You can assemble different releases with different configuration per application or even with different applications altogether.

### Hooks and Configuration

Releases also provide built-in hooks for configuring almost every need of the production system:

  * `config/config.exs` (and `config/prod.exs`) - provides build-time application configuration, which is executed when the release is assembled

  * `config/releases.exs` - provides runtime application configuration. It is executed every time the release boots and is further extensible via config providers

  * `rel/vm.args.eex` - a template file that is copied into every release and provides static configuration of the Erlang Virtual Machine and other runtime flags

  * `rel/env.sh.eex` and `rel/env.bat.eex` - template files that are copied into every release and executed on every command to set up environment variables, including ones specific to the VM, and the general environment

We have written extensive documentation on releases, so we recommend checking it out for more information.

## Configuration overhaul

A new `Config` module has been added to Elixir. The previous configuration API, `Mix.Config`, was part of the Mix build tool. But since releases provide runtime configuration and Mix is not included in releases, we ported the `Mix.Config` API to Elixir. In other words, `use Mix.Config` has been soft-deprecated in favor of `import Config`.

Another important change related to configuration is that `mix new` will no longer generate a `config/config.exs` file. [Relying on configuration is undesired for most libraries](https://hexdocs.pm/elixir/library-guidelines.html#avoid-application-configuration) and the generated config files pushed library authors in the wrong direction. Furthermore, `mix new --umbrella` will no longer generate a configuration for each child app, instead all configuration should be declared in the umbrella root. That's how it has always behaved, we are now making it explicit.

## Other enhancements

There are many other enhancements. The Elixir CLI got a handful of new options in order to best support releases. `Logger` now computes its sync/async/discard thresholds in a decentralized fashion, reducing contention. `EEx` templates support more complex expressions than before. Finally, there is a new `~U` sigil for working with UTC DateTimes as well as new functions in the `File`, `Registry`, and `System` modules.

## v1.9.1 (2019-07-18)

### 1. Enhancements

#### Mix

  * [mix format] Print relative paths in `--check-formatted` output
  * [mix release] Support included applications

### 2. Bug fixes

#### Elixir

  * [Code] Fix formatter wrongly removing nested parens in nested calls

#### Logger

  * [Logger] Do not crash translator on poorly formatted supervisor names

#### Mix

  * [mix compile] Raise readable error for mismatched sources during compilation
  * [mix release] Preserve UTF8 encoding in release config files

## v1.9.0 (2019-06-24)

### 1. Enhancements

#### EEx

  * [EEx] Allow more complex mixed expressions when tokenizing

#### Elixir

  * [Access] Allow `Access.at/1` to handle negative index
  * [CLI] Add support for `--boot`, `--boot-var`, `--erl-config`, `--pipe-to`, `--rpc-eval`, and `--vm-args` options
  * [Code] Add `static_atom_encoder` option to `Code.string_to_quoted/2`
  * [Code] Support `:force_do_end_blocks` on `Code.format_string!/2` and `Code.format_file!/2`
  * [Code] Do not raise on deadlocks on `Code.ensure_compiled/1`
  * [Config] Add `Config`, `Config.Reader`, and `Config.Provider` modules for working with configuration
  * [File] Add `File.rename!/2`
  * [Inspect] Add `:inspect_fun` and `:custom_options` to `Inspect.Opts`
  * [Kernel] Add `~U` sigil for UTC date times
  * [Kernel] Optimize `&super/arity` and `&super(&1)`
  * [Kernel] Optimize generated code for `with` with a catch-all clause
  * [Kernel] Validate `__struct__` key in map returned by `__struct__/0,1`
  * [Module] Add `Module.get_attribute/3`
  * [Protocol] Improve `Protocol.UndefinedError` messages to also include the type that was attempted to dispatch on
  * [Protocol] Optimize performance of dynamic dispatching for non-consolidated protocols
  * [Record] Include field names in generated type for records
  * [Regex] Automatically recompile regexes
  * [Registry] Add `Registry.select/2`
  * [System] Add `System.restart/0`, `System.pid/0` and `System.no_halt/1`
  * [System] Add `System.get_env/2`, `System.fetch_env/1`, and `System.fetch_env!/1`
  * [System] Support `SOURCE_DATE_EPOCH` for reproducible builds

#### ExUnit

  * [ExUnit] Allow multiple `:exclude` on configuration/CLI
  * [ExUnit.DocTest] No longer wrap doctest errors in custom exceptions. They ended-up hiding more information than showing
  * [ExUnit.DocTest] Display the actual doctest code when doctest fails

#### IEx

  * [IEx.CLI] Copy ticktime from remote node on IEx `--remsh`
  * [IEx.CLI] Automatically add a host on node given to `--remsh`

#### Logger

  * [Logger] Use a decentralized mode computation for Logger which allows overloads to be detected more quickly
  * [Logger] Use `persistent_term` to store configuration whenever available for performance

#### Mix

  * [Mix] Follow XDG base dir specification in Mix for temporary and configuration files
  * [Mix.Generator] Add `copy_file/3`, `copy_template/4`, and `overwite?/2`
  * [Mix.Project] Add `preferred_cli_target` that works like `preferred_cli_env`
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
  * [Code] Do not crash formatter on false positive sigils
  * [Enum] Ensure the first equal entry is returned by `Enum.min/2` and `Enum.max/2`
  * [Kernel] Improve error message when string interpolation is used in a guard
  * [Kernel] Properly merge and handle docs for callbacks with multiple clauses
  * [Kernel] Guarantee reproducible builds on modules with dozens of specs
  * [Kernel] Resolve `__MODULE__` accordingly in nested `defmodule` to avoid double nesting
  * [Kernel] Type variables starting with an underscore (`_foo`) should not raise compile error
  * [Kernel] Keep order of elements when macro `in/2` is expanded with a literal list on the right-hand side
  * [Kernel] Print proper location on undefined function error from dynamically generated functions
  * [Kernel] **Potentially breaking** Do not leak aliases when nesting module definitions that are fully namespaced modules. If you defined `defmodule Elixir.Foo.Bar` inside `defmodule Foo`, previous Elixir versions would automatically define an alias, but fully namespaced modules such as `Elixir.Foo.Bar` should never define or require an alias. If you were accidentally relying on this broken behaviour, your code may no longer work
  * [System] Make sure `:init.get_status/0` is set to `{:started, :started}` once the system starts
  * [Path] Do not expand `~` in `Path.expand/2` when not followed by a path separator
  * [Protocol] Ensure `debug_info` is kept in protocols
  * [Regex] Ensure inspect returns valid `~r//` expressions when they are manually compiled with backslashes
  * [Registry] Fix ETS leak in `Registry.register/2` for already registered calls in unique registries while the process is still alive

#### ExUnit

  * [ExUnit] Raise error if attempting to run single line tests on multiple files
  * [ExUnit] Return proper error on duplicate child IDs on `start_supervised`

#### IEx

  * [IEx] Automatically shut down IEx if we receive EOF

#### Logger

  * [Logger] Don't discard Logger messages from other nodes as to leave a trail on both systems

#### Mix

  * [mix compile] Ensure Erlang-based Mix compilers (erlang, leex, yecc) set valid position on diagnostics
  * [mix compile] Ensure compilation halts in an umbrella project if one of the siblings fail to compile
  * [mix deps] Raise an error if the umbrella app's dir name and `mix.exs` app name don't match
  * [mix deps.compile] Fix subcommand splitting bug in rebar3
  * [mix test] Do not consider modules that are no longer cover compiled when computing coverage report, which could lead to flawed reports

### 3. Soft-deprecations (no warnings emitted)

#### Mix

  * [Mix.Config] `Mix.Config` has been deprecated in favor of the `Config` module that now ships as part of Elixir itself. Reading configuration files should now be done by the `Config.Reader` module

### 4. Hard-deprecations

#### Elixir

  * [CLI] Deprecate `--detached` option, use `--erl "-detached"` instead
  * [Map] Deprecate Enumerable keys in `Map.drop/2`, `Map.split/2`, and `Map.take/2`
  * [String] The `:insert_replaced` option in `String.replace/4` has been deprecated. Instead you may pass a function as a replacement or use `:binary.replace/4` if you need to support earlier Elixir versions

#### Mix

  * [Mix.Project] Deprecate `Mix.Project.load_paths/1` in favor of `Mix.Project.compile_path/1`

## v1.8

The CHANGELOG for v1.8 releases can be found [in the v1.8 branch](https://github.com/elixir-lang/elixir/blob/v1.8/CHANGELOG.md).
