# Changelog for Elixir v1.14

Elixir v1.14 requires Erlang/OTP 23+ with a small batch of new features
and the usual enhancements and bug fixes to Elixir and its standard library.
We cover the most notable changes next.

## PartitionSupervisor

`PartitionSupervisor` is a new module that implements a new supervisor type. The
partition supervisor is designed to help with situations where you have a single
supervised process that becomes a bottleneck. If that process's state can be
easily partitioned, then you can use `PartitionSupervisor` to supervise multiple
isolated copies of that process running concurrently, each assigned its own
partition.

For example, imagine you have a `ErrorReporter` process that you use to report
errors to a monitoring service.

```elixir
# Application supervisor:
children = [
  # ...,
  ErrorReporter
]

Supervisor.start_link(children, strategy: :one_for_one)
```

As the concurrency of your application goes up, the `ErrorReporter` process
might receive requests from many other processes and eventually become a
bottleneck. In a case like this, it could help to spin up multiple copies of the
`ErrorReporter` process under a `PartitionSupervisor`.

```elixir
# Application supervisor
children = [
  {PartitionSupervisor, child_spec: ErrorReporter, name: Reporters}
]
```

The `PartitionSupervisor` will spin up a number of processes equal to
`System.schedulers_online()` by default (most often one per core). Now, when
routing requests to `ErrorReporter` processes we can use a `:via` tuple and
route the requests through the partition supervisor.

```elixir
partitioning_key = self()
ErrorReporter.report({:via, PartitionSupervisor, {Reporters, partitioning_key}}, error)
```

Using `self()` as the partitioning key here means that the same process will
always report errors to the same `ErrorReporter` process, ensuring a form of
back-pressure. You can use any term as the partitioning key.

### A Common Example

A common and practical example of a good use case for `PartitionSupervisor` is
partitioning something like a `Task.Supervisor`. With many tasks under it, a
task supervisor can be a bottleneck. Instead of starting a single
`Task.Supervisor` for a set of tasks, you can start multiple ones:

```elixir
children = [
  {PartitionSupervisor, child_spec: Task.Supervisor, name: MyApp.TaskSupervisors}
]

Supervisor.start_link(children, strategy: :one_for_one)
```

Now you start tasks on the task supervisor for the right partition. For
instance, you can partition by PID, like in the previous example:

```elixir
Task.Supervisor.async(
  {:via, PartitionSupervisor, {MyApp.TaskSupervisors, self()}},
  fn -> :do_some_work end
)
```

## Improved errors on binaries and evaluation

TODO.

## Slicing with steps

TODO.

## Expression-based inspection

TODO.

## v1.14.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Support multi-line comments to EEx via `<%!-- --%>`
  * [EEx] Add `EEx.tokenize/2`

#### Elixir

  * [Calendar] Support ISO8601 basic format parsing with `DateTime.from_iso8601/2`
  * [Code] Emit deprecation and type warnings for invalid options in on `Code.compile_string/2` and `Code.compile_quoted/2`
  * [Code] Warn if an outdated lexical tracker is given on eval
  * [Code] Add `Code.env_for_eval/1` and `Code.eval_quoted_with_env/3`
  * [Code] Improve stacktraces from eval operations on Erlang/OTP 25+
  * [Enum] Allow slicing with steps in `Enum.slice/2`
  * [Float] Do not show floats in scientific notation if below `1.0e16` and the fractional value is precisely zero
  * [Inspect] Improve error reporting when there is a faulty implementation of the `Inspect` protocol
  * [Inspect] Skip fields set to their default value when inspecting structs
  * [Inspect] Use expression-based inspection for `Date.Range`, `MapSet`, and `Version.Requirement`
  * [Kernel] Allow any guard expression as the size of a bitstring in a pattern match
  * [Kernel] Allow composite types with pins as the map key in a pattern match
  * [Kernel] Print escaped version of control chars when they show up as unexpected tokens
  * [Kernel] Warn on confusable non-ASCII identifiers
  * [Kernel] Add `..` as a nullary operator that returns `0..-1//1`
  * [Kernel] Implement Unicode Technical Standard #39 recommendations; in particular, we warn for confusable scripts and restrict identifiers to single-scripts or highly restrictive mixed-scripts
  * [Kernel] Add `binary_slice/2` and `binary_slice/3`
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
  * [String] Allow slicing with steps in `String.slice/2`
  * [Task] Add `:zip_input_on_exit` option to `Task.async_stream/3`
  * [Task] Store `:mfa` in the `Task` struct for reflection purposes
  * [URI] Add `URI.append_query/2`
  * [Version] Add `Version.to_string/1`
  * [Version] Colorize `Version.Requirement` source in the `Inspect` protocol

#### ExUnit

  * [ExUnit] Add `ExUnit.Callbacks.start_link_supervised!/2`
  * [ExUnit] Add `ExUnit.run/1` to rerun test modules

#### IEx

  * [IEx] Evaluate `--dot-iex` line by line
  * [IEx.Helpers] Allow an atom to be given to `pid/1`

#### Logger

  * [Logger] Add `Logger.put_process_level/2`

#### Mix

  * [mix do] Support `--app` option to restrict recursive tasks in umbrella projects
  * [mix do] Allow using `+` as a task separator instead of comma
  * [mix new] Do not allow projects to be created with application names that conflict with multi-arg Erlang VM switches
  * [mix profile] Return the return value of the profiled function
  * [mix release] Make BEAM compression opt-in
  * [mix test] Improve error message when suite fails due to coverage
  * [mix test] Support `:test_elixirc_options` and default to not generating docs nor debug info chunk for tests

### 2. Bug fixes

#### Elixir

  * [CLI] Improve errors on incorrect `--rpc-eval` usage
  * [Code] Do not emit warnings when formatting code
  * [Enum] Allow slices to overflow on both starting and ending positions
  * [Kernel] Do not allow restricted characters in identifiers according to UTS39
  * [Kernel] Define `__exception__` field as `true` when expanding exceptions in typespecs
  * [Kernel] Warn if any of `True`, `False`, and `Nil` aliases are used
  * [Kernel] Warn on underived `@derive` attributes
  * [Protocol] Warn if a protocol has no definitions
  * [String] Allow slices to overflow on both starting and ending positions

#### ExUnit

  * [ExUnit] Do not raise when diffing unknown bindings in guards
  * [ExUnit] Properly print diffs when comparing improper lists with strings at the tail position
  * [ExUnit] Add short hash to `tmp_dir` in ExUnit to avoid test name collision
  * [ExUnit] Do not store logs in the CLI formatter (this reduces memory usage for suites with `capture_log`)
  * [ExUnit] Run `ExUnit.after_suite/1` callback even when no tests run
  * [ExUnit] Fix scenario where `setup` with imported function from within `describe` failed to compile

#### Mix

  * [mix compile.elixir] Fix `--warnings-as-errors` when used with `--all-warnings`
  * [mix format] Do not add new lines if the formatted file is empty
  * [mix release] Only set `RELEASE_MODE` after `env.{sh,bat}` are executed

#### IEx

  * [IEx] Disallow short-hand pipe after matches

### 3. Soft deprecations (no warnings emitted)

#### EEx

  * [EEx] Using `<%# ... %>` for comments is deprecated. Please use `<% # ... %>` or the new multi-line comments with `<%!-- ... --%>`

#### Logger

  * [Logger] Deprecate `Logger.enable/1` and `Logger.disable/1` in favor of `Logger.put_process_level/2`

#### Mix

  * [mix cmd] The `--app` option in `mix cmd CMD` is deprecated in favor of the more efficient `mix do --app app cmd CMD`

### 4. Hard deprecations

#### Elixir

  * [Application] Calling `Application.get_env/3` and friends in the module body is now discouraged, use `Application.compile_env/3` instead
  * [Bitwise] `use Bitwise` is deprecated, use `import Bitwise` instead
  * [Bitwise] `~~~` is deprecated in favor of `bnot` for clarity
  * [Kernel.ParallelCompiler] Returning a list or two-element tuple from `:each_cycle` is deprecated, return a `{:compile | :runtime, modules, warnings}` tuple instead
  * [Kernel] Deprecate the operator `<|>` to avoid ambiguity with upcoming extended numerical operators
  * [String] Deprecate passing a binary compiled pattern to `String.starts_with?/2`

#### Logger

  * [Logger] Deprecate `$levelpad` on message formatting

#### Mix

  * [Mix] `Mix.Tasks.Xref.calls/1` is deprecated in favor of compilation tracers

### 5. Backwards incompatible changes

#### Mix

  * [mix local.rebar] Remove support for rebar2, which has not been updated in 5 years, and is no longer supported on recent Erlang/OTP versions

## v1.13

The CHANGELOG for v1.13 releases can be found [in the v1.13 branch](https://github.com/elixir-lang/elixir/blob/v1.13/CHANGELOG.md).
