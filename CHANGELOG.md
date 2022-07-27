# Changelog for Elixir v1.14

Elixir v1.14 brings many improvements to the debugging experience in Elixir
and data-type inspection. It also includes a new abstraction for easy
partitioning of processes called `PartitionSupervisor`, as well as improved
compilation times and error messages.

Elixir v1.14 is the last version to support Erlang/OTP 23. Consider updating
to Erlang/OTP 24 or Erlang/OTP 25.

## `dbg`

`Kernel.dbg/2` is a new macro that's somewhat similar to `IO.inspect/2`, but
specifically tailored for **debugging**.

When called, it prints the value of whatever you pass to it, plus the debugged
code itself as well as its location. This code:

```elixir
# In my_file.exs
feature = %{name: :dbg, inspiration: "Rust"}
dbg(feature)
dbg(Map.put(feature, :in_version, "1.14.0"))
```

Prints this:

```shell
$ elixir my_file.exs
[my_file.exs:2: (file)]
feature #=> %{inspiration: "Rust", name: :dbg}

[my_file.exs:3: (file)]
Map.put(feature, :in_version, "1.14.0") #=> %{in_version: "1.14.0", inspiration: "Rust", name: :dbg}
```

`dbg/2` can do more. It's a macro, so it *understands Elixir code*. You can see
that when you pass a series of `|>` pipes to it. `dbg/2` will print the value
for every step of the pipeline. This code:

```elixir
# In dbg_pipes.exs
__ENV__.file
|> String.split("/", trim: true)
|> List.last()
|> File.exists?()
|> dbg()
```

Prints this:

```shell
$ elixir dbg_pipes.exs
[dbg_pipes.exs:5: (file)]
__ENV__.file #=> "/home/myuser/dbg_pipes.exs"
|> String.split("/", trim: true) #=> ["home", "myuser", "dbg_pipes.exs"]
|> List.last() #=> "dbg_pipes.exs"
|> File.exists?() #=> true
```

### IEx and Prying

`dbg/2` supports configurable backends. IEx automatically replaces the default
backend by one that halts the code execution with `IEx.Pry`, giving developers
the option to access local variables, imports, and more. This also works with
pipelines: if you pass a series of `|>` pipe calls to `dbg` (or pipe into it at the
end, like `|> dbg()`), you'll be able to step through every line in the pipeline.

You can keep the default behaviour by passing the `--no-pry` option to IEx.

## PartitionSupervisor

`PartitionSupervisor` is a new module that implements a new supervisor type. The
partition supervisor is designed to help with situations where you have a single
supervised process that becomes a bottleneck. If that process's state can be
easily partitioned, then you can use `PartitionSupervisor` to supervise multiple
isolated copies of that process running concurrently, each assigned its own
partition.

For example, imagine you have an `ErrorReporter` process that you use to report
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
partitioning something like a `DynamicSupervisor`. When starting many processes
under it, a dynamic supervisor can be a bottleneck, especially if said processes
take a long time to initialize. Instead of starting a single `DynamicSupervisor`,
you can start multiple:

```elixir
children = [
  {PartitionSupervisor, child_spec: DynamicSupervisor, name: MyApp.DynamicSupervisors}
]

Supervisor.start_link(children, strategy: :one_for_one)
```

Now you start processes on the dynamic supervisor for the right partition.
For instance, you can partition by PID, like in the previous example:

```elixir
DynamicSupervisor.start_child(
  {:via, PartitionSupervisor, {MyApp.DynamicSupervisors, self()}},
  my_child_specification
)
```

## Improved errors on binaries and evaluation

Erlang/OTP 25 improved errors on binary construction and evaluation. These improvements
apply to Elixir as well. Before v1.14, errors when constructing binaries would
often be hard-to-debug generic "argument errors". With Erlang/OTP 25 and Elixir v1.14,
more detail is provided for easier debugging. This work is part of [EEP
54](https://www.erlang.org/eeps/eep-0054).

Before:

```elixir
int = 1
bin = "foo"
int <> bin
#=> ** (ArgumentError) argument error
```

Now:

```elixir
int = 1
bin = "foo"
int <> bin
#=> ** (ArgumentError) construction of binary failed:
#=>    segment 1 of type 'binary':
#=>    expected a binary but got: 1
```

## Slicing with steps

Elixir v1.12 introduced **stepped ranges**, which are ranges where you can
specify the "step":

```elixir
Enum.to_list(1..10//3)
#=> [1, 4, 7, 10]
```

Stepped ranges are particularly useful for numerical operations involving
vectors and matrices (see [Nx](https://github.com/elixir-nx/nx), for example).
However, the Elixir standard library was not making use of stepped ranges in its
APIs. Elixir v1.14 starts to take advantage of steps with support for stepped
ranges in a couple of functions. One of them is `Enum.slice/2`:

```elixir
letters = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
Enum.slice(letters, 0..5//2)
#=> ["a", "c", "e"]
```

`binary_slice/2` (and `binary_slice/3` for completeness) has been added to the
`Kernel` module, that works with bytes and also support stepped ranges:

```elixir
binary_slice("Elixir", 1..5//2)
#=> "lx"
```

## Expression-based inspection and `Inspect` improvements

In Elixir, it's conventional to implement the `Inspect` protocol for opaque
structs so that they're inspected with a special notation, resembling this:

```elixir
MapSet.new([:apple, :banana])
#MapSet<[:apple, :banana]>
```

This is generally done when the struct content or part of it is private and the
`%name{...}` representation would reveal fields that are not part of the public
API.

The downside of the `#name<...>` convention is that *the inspected output is not
valid Elixir code*. For example, you cannot copy the inspected output and paste
it into an IEx session.

Elixir v1.14 changes the convention for some of the standard-library structs.
The `Inspect` implementation for those structs now returns a string with a valid
Elixir expression that recreates the struct when evaluated. In the `MapSet`
example above, this is what we have now:

```elixir
fruits = MapSet.new([:apple, :banana])
MapSet.put(fruits, :pear)
#=> MapSet.new([:apple, :banana, :pear])
```

The `MapSet.new/1` expression evaluates to exactly the struct that we're
inspecting. This allows us to hide the internals of `MapSet`, while keeping
it as valid Elixir code. This expression-based inspection has been
implemented for `Version.Requirement`, `MapSet`, and `Date.Range`.

Finally, we have improved the `Inspect` protocol for structs so that
fields are inspected in the order they are declared in `defstruct`.
The option `:optional` has also been added when deriving the `Inspect`
protocol, giving developers more control over the struct representation.
See the updated documentation for `Inspect` for a general rundown on
the approaches and options available.

## v1.14.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Support multi-line comments to EEx via `<%!-- --%>`
  * [EEx] Add `EEx.tokenize/2`

#### Elixir

  * [Access] Add `Access.slice/1`
  * [Application] Add `Application.compile_env/4` and `Application.compile_env!/3` to read the compile-time environment inside macros
  * [Calendar] Support ISO8601 basic format parsing with `DateTime.from_iso8601/2`
  * [Calendar] Add `day`/`hour`/`minute` on `add`/`diff` across different calendar modules
  * [Code] Add `:normalize_bitstring_modifiers` to `Code.format_string!/2`
  * [Code] Emit deprecation and type warnings for invalid options in on `Code.compile_string/2` and `Code.compile_quoted/2`
  * [Code] Warn if an outdated lexical tracker is given on eval
  * [Code] Add `Code.env_for_eval/1` and `Code.eval_quoted_with_env/3`
  * [Code] Improve stacktraces from eval operations on Erlang/OTP 25+
  * [Code.Fragment] Add support for `__MODULE__` in several functions
  * [Code.Fragment] Support surround and context suggestions across multiple lines
  * [Enum] Allow slicing with steps in `Enum.slice/2`
  * [File] Support `dereference_symlinks: true` in `File.cp/3` and `File.cp_r/3`
  * [Float] Do not show floats in scientific notation if below `1.0e16` and the fractional value is precisely zero
  * [Float] Add `Float.min_finite/0` and `Float.max_finite/0`
  * [Inspect] Improve error reporting when there is a faulty implementation of the `Inspect` protocol
  * [Inspect] Allow `:optional` when deriving the Inspect protocol for hiding fields that match their default value
  * [Inspect] Inspect struct fields in the order they are declared in `defstruct`
  * [Inspect] Use expression-based inspection for `Date.Range`, `MapSet`, and `Version.Requirement`
  * [IO] Support `Macro.Env` and keywords as stacktrace definitions in `IO.warn/2`
  * [IO] Add `IO.ANSI.syntax_colors/0` and related configuration to be shared across IEx and `dbg`
  * [Kernel] Add new `dbg/0-2` macro
  * [Kernel] Allow any guard expression as the size of a bitstring in a pattern match
  * [Kernel] Allow composite types with pins as the map key in a pattern match
  * [Kernel] Print escaped version of control chars when they show up as unexpected tokens
  * [Kernel] Warn on confusable non-ASCII identifiers
  * [Kernel] Add `..` as a nullary operator that returns `0..-1//1`
  * [Kernel] Implement Unicode Technical Standard #39 recommendations. In particular, we warn for confusable scripts and restrict identifiers to single-scripts or highly restrictive mixed-scripts
  * [Kernel] Automatically perform NFC conversion of identifiers
  * [Kernel] Add `binary_slice/2` and `binary_slice/3`
  * [Kernel] Lazily expand module attributes to avoid compile-time deps
  * [Kernel] Automatically cascade `generated: true` annotations on macro expansion
  * [Keyword] Add `Keyword.from_keys/2` and `Keyword.replace_lazy/3`
  * [List] Add `List.keysort/3` with support for a `sorter` function
  * [Macro] Add `Macro.classify_atom/1` and `Macro.inspect_atom/2`
  * [Macro] Add `Macro.expand_literal/2` and `Macro.path/2`
  * [Macro.Env] Add `Macro.Env.prune_compile_info/1`
  * [Map] Add `Map.from_keys/2` and `Map.replace_lazy/3`
  * [MapSet] Add `MapSet.filter/2`, `MapSet.reject/2`, and `MapSet.symmetric_difference/2`
  * [Node] Add `Node.spawn_monitor/2` and `Node.spawn_monitor/4`
  * [Module] Support new `@after_verify` attribute for executing code whenever a module is verified
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
  * [ExUnit] Colorize summary in yellow with message when all tests are excluded
  * [ExUnit] Display friendly error when test name is too long

#### IEx

  * [IEx] Evaluate `--dot-iex` line by line
  * [IEx] Add line-by-line evaluation of IEx breakpoints
  * [IEx.Autocomplete] Autocomplete bitstrings modifiers (after `::` inside `<<...>>`)
  * [IEx.Helpers] Allow an atom to be given to `pid/1`

#### Logger

  * [Logger] Add `Logger.put_process_level/2`

#### Mix

  * [mix compile] Add `--no-optional-deps` to skip optional dependencies to test compilation works without optional dependencies
  * [mix compile] Include column information on error diagnostics when possible
  * [mix deps] `Mix.Dep.Converger` now tells which deps formed a cycle
  * [mix do] Support `--app` option to restrict recursive tasks in umbrella projects
  * [mix do] Allow using `+` as a task separator instead of comma
  * [mix format] Support filename in `mix format -` when reading from stdin
  * [mix format] Compile if `mix format` plugins are missing
  * [mix new] Do not allow projects to be created with application names that conflict with multi-arg Erlang VM switches
  * [mix profile] Return the return value of the profiled function
  * [mix release] Make BEAM compression opt-in
  * [mix release] Let `:runtime_config_path` accept `false` to skip the `config/runtime.exs`
  * [mix test] Improve error message when suite fails due to coverage
  * [mix test] Support `:test_elixirc_options` and default to not generating docs nor debug info chunk for tests
  * [mix xref] Support `--group` flag in `mix xref graph`

### 2. Bug fixes

#### Elixir

  * [Calendar] Handle widths with "0" in them in `Calendar.strftime/3`
  * [CLI] Improve errors on incorrect `--rpc-eval` usage
  * [CLI] Return proper exit code on Windows
  * [Code] Do not emit warnings when formatting code
  * [Enum] Allow slices to overflow on both starting and ending positions
  * [Kernel] Do not allow restricted characters in identifiers according to UTS39
  * [Kernel] Define `__exception__` field as `true` when expanding exceptions in typespecs
  * [Kernel] Warn if any of `True`, `False`, and `Nil` aliases are used
  * [Kernel] Warn on underived `@derive` attributes
  * [Kernel] Remove compile-time dependency from `defimpl :for`
  * [Kernel] Track all arities on imported functions
  * [Protocol] Warn if a protocol has no definitions
  * [Regex] Show list options when inspecting a Regex manually defined with `Regex.compile/2`
  * [String] Allow slices to overflow on both starting and ending positions

#### ExUnit

  * [ExUnit] Do not crash when diffing unknown bindings in guards
  * [ExUnit] Properly print diffs when comparing improper lists with strings at the tail position
  * [ExUnit] Add short hash to `tmp_dir` in ExUnit to avoid test name collision
  * [ExUnit] Do not store logs in the CLI formatter (this reduces memory usage for suites with `capture_log`)
  * [ExUnit] Run `ExUnit.after_suite/1` callback even when no tests run
  * [ExUnit] Fix scenario where `setup` with imported function from within `describe` failed to compile

#### IEx

  * [IEx] Disallow short-hand pipe after matches
  * [IEx] Fix `exports/1` in IEx for long function names

#### Mix

  * [mix compile.elixir] Fix `--warnings-as-errors` when used with `--all-warnings`
  * [mix compile.elixir] Ensure semantic recompilation cascades to path dependencies
  * [mix compile.elixir] Lock the compiler to avoid concurrent usage
  * [mix format] Do not add new lines if the formatted file is empty
  * [mix release] Only set `RELEASE_MODE` after `env.{sh,bat}` are executed
  * [mix release] Allow application mode configuration to cascade to dependencies
  * [mix xref] Do not emit already consolidated warnings during `mix xref trace`
  * [Mix] Do not start apps with `runtime: false` on `Mix.install/2`

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [File] Passing a callback as third argument to `File.cp/3` and `File.cp_r/3` is deprecated.
    Instead pass the callback the `:on_conflict` key of a keyword list

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
