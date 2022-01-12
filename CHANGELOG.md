# Changelog for Elixir v1.14

Elixir v1.14 requires Erlang/OTP 23+.

## v1.14.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Support multi-line comments to EEx via `<%!-- --%>`

#### Elixir

  * [Code] Emit deprecation and type warnings on `Code.compile_string/2` and `Code.compile_quoted/2`
  * [Code] Warn if an outdated lexical tracker is given on eval
  * [Code] Add `Code.env_for_eval/1` and `Code.eval_quoted_with_env/3`
  * [Inspect] Improve error reporting when there is a faulty inspect implementation
  * [Kernel] Allow any guard expression as the size of a bitstring in a pattern match
  * [Kernel] Allow composite types with pins as the map key in a pattern match
  * [Kernel] Print escaped version of control chars when they show up as unexpected tokens
  * [Keyword] Add `Keyword.from_keys/2` and `Keyword.replace_lazy/3`
  * [List] Add `List.keysort/3` with support for a `sorter` function
  * [Macro] Add `Macro.classify_atom/1` and `Macro.inspect_atom/2`
  * [Macro.Env] Add `Macro.Env.prune_compile_info/1`
  * [Map] Add `Map.from_keys/2` and `Map.replace_lazy/3`
  * [MapSet] Add `MapSet.filter/2` and `MapSet.reject/2`
  * [Node] Add `Node.spawn_monitor/2` and `Node.spawn_monitor/4`
  * [PartitionSupervisor] Add `PartitionSupervisor` that starts multiple isolated partitions of the same child for scalability
  * [Path] Add `Path.safe_relative/1` and `Path.safe_relative_to/2`
  * [Registry] Add `Registry.count_select/2`
  * [Stream] Add `Stream.duplicate/2` and `Stream.transform/5`
  * [String] Support empty lookup lists in `String.replace/3`, `String.split/3`, and `String.splitter/3`
  * [Version] Add `Version.to_string/1`
  * [Version] Colorize `Version.Requirement` source in Inspect protocol

#### IEx

  * [IEx.Helpers] Allow an atom to be given to `pid/1`

#### Mix

  * [mix test] Improve error message when suite fails due to coverage

### 2. Bug fixes

#### Elixir

  * [Kernel] Define `__exception__` field as true when expanding exceptions in typespecs

#### ExUnit

  * [ExUnit] Do not raise when diffing unknown bindings in guards

#### IEx

  * [IEx] Disallow short-hand pipe after matches

### 3. Soft-deprecations (no warnings emitted)

#### EEx

  * [EEx] Using `<%# ... %>` for comments is deprecated. Please use `<% # ... %>` or the new multi-line comments with `<%!-- ... --%>`

### 4. Hard-deprecations

#### Elixir

  * [Application] Calling `Application.get_env/3` and friends in the module body is now discouraged, use `Application.compile_env/3` instead
  * [Bitwise] `use Bitwise` is deprecated, use `import Bitwise` instead
  * [Bitwise] `~~~` is deprecated in favor of `bnot` for clarity
  * [Kernel.ParallelCompiler] Returning a list or two-element tuple from `:each_cycle` is deprecated, return a `{:compile | :runtime, modules, warnings}` tuple instead
  * [Kernel] Deprecate the operator `<|>` to avoid ambiguity with upcoming extended numerical operators
  * [String] Deprecate passing a binary compiled pattern to `String.starts_with?/2`

#### Mix

  * [Mix] `Mix.Tasks.Xref.calls/1` is deprecated in favor of compilation tracers

## v1.13

The CHANGELOG for v1.13 releases can be found [in the v1.13 branch](https://github.com/elixir-lang/elixir/blob/v1.13/CHANGELOG.md).
