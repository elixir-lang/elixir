# Changelog for Elixir v1.18

TODO.

## Type system improvements

* Type checking and inference of patterns

* [Support for tuples and lists as composite types](https://elixir-lang.org/blog/2024/08/28/typing-lists-and-tuples/) as well as type checking of its basic operations from Kernel

* Type checking of all built-in conversion functions, such as `List.to_integer/1` and `Atom.to_string/1`

* Type checking of all functions in the `Tuple` module

## ExUnit improvements

ExUnit now supports parameterized tests to run the same test module multiple times under different parameters.

For example, Elixir ships a local, decentralized and scalable key-value process storage called `Registry`. The registry can be partitioned and its implementation differs depending if partitioning is enabled or not. Therefore, during tests, we want to ensure both modes are exercised. With Elixir v1.18, we can achieve this by writing:

```elixir
defmodule Registry.Test do
  use ExUnit.Case,
    async: true,
    parameterize: [
      %{partitions: 1},
      %{partitions: 8}
    ]

  # ... the actual tests ...
end
```

ExUnit parameterizes whole test modules. If your modules are configured to run concurrently, as above, so will the parameterized ones.

ExUnit also comes with the ability of specifying test groups. While ExUnit supports running tests concurrently, those tests must not have shared state between them. However, in large applications, it may be common for some tests to depend on some shared state, and other tests to depend on a completely separate state. For example, part of your tests may depend on Cassandra, while others depend on Redis. Prior to Elixir v1.18, these tests could not run concurrently, but in v1.18 they might as long as they are assigned to different groups. Tests modules within the same group do not run concurrently, but across groups, they might.

With features like async tests, suite partitioning, and now grouping, Elixir developers have plenty of flexibility to make the most use of their machine resources, both in development and in CI.

## `mix format --migrate`

TODO.

## Potential incompatibilities

This release no longer supports WERL (a graphical user interface on Windows used by Erlang 25 and earlier). For a better user experience on Windows terminals, use Erlang/OTP 26+.

Furthermore, in order to support inference of patterns, Elixir will raise if it finds recursive variable definitions. This means patterns that never match, such as this one, will no longer compile:

    def foo(x = {:ok, y}, x = y)

However, recursion of root variables (where variables directly point to each other), will also fail to compile:

    def foo(x = y, y = z, z = x)

While the definition above could succeed (as long as all three arguments are equal), the cycle is not necessary and could be removed, as below:

    def foo(x = y, y = z, z)

You may also prever to write using guards:

    def foo(x, y, z) when x == y and y == z

## v1.18.0-dev

### 1. Enhancements

#### Elixir

  * [CLI] Add experimental PowerShell scripts for `elixir`, `elixirc`, and `mix` on Windows. Those provide a safer entry point for running Elixir from other platforms
  * [Code] Support several migration options in `Code.format_string!/2`
  * [Code] Add parenthesis around `--` and `---` in `Code.format_string!/2` to make precedence clearer
  * [Code.Fragment] Have `:capture_arg` as its own entry in `Code.Fragment.surround_context/2`
  * [Config] Add `Config.read_config/1`
  * [Enumerable] Add `Enum.product_by/2` and `Enum.sum_by/2`
  * [Exception] Add `MissingApplicationsError` exception to denote missing applications
  * [Kernel] Update source code parsing to match [UTS #55](https://www.unicode.org/reports/tr55/) latest recommendations. In particular, mixed script is allowed in identifiers as long as they are separate by underscores (`_`), such as `http_сервер`. Previously allowed highly restrictive identifiers, which mixed Latin and other scripts, such as the japanese word for t-shirt, `Tシャツ`, now require the underscore as well
  * [Kernel] Warn on bidirectional confusability in identifiers
  * [Kernel] Now verify the type of the binary generators
  * [Kernel] Track the type of tuples in patterns and inside `elem/2`
  * [List] Add `List.ends_with?/2`
  * [Macro] Improve `dbg` handling of `if/2`, `with/1` and of code blocks
  * [Process] Handle arbitrarily high integer values in `Process.sleep/1`
  * [String] Inspect special whitespace and zero-width characters using their Unicode representation

#### ExUnit

  * [ExUnit] Support parameterized tests on `ExUnit.Case`
  * [ExUnit] Support test groups: tests in the same group never run concurrently

#### IEx

  * [IEx] Add `:dot_iex` support to `IEx.configure/1`
  * [IEx] Add report for normal/shutdown exits in IEx

#### Mix

  * [mix compile] Ensure only a single operating system process can compile at a given time
  * [mix deps.get] Ensure only a single operating system process can fetch deps at a given time
  * [mix format] Add `mix format --migrate` to migrate from deprecated functionality
  * [mix test] Taint failure manifest if requiring or compiling tests fail
  * [Mix.Project] Add a `:listeners` configuration to listen to compilation events from the current and other operating system processes
  * [Mix.Task.Compiler] Add API for fetching all persisted compiler diagnostics
  * [Mix.Task.Compiler] Add API for fetching all compiler tasks

### 2. Bug fixes

#### Elixir

  * [Code.Fragment] Properly handle keyword keys as their own entry
  * [Inspect.Algebra] Ensure `next_break_fits` respects `line_length`
  * [Module] Include module attribute line and name when tracing its aliases

#### ExUnit

  * [ExUnit.Assertions] Raise if guards are used in `assert/1` with `=`

#### IEx

  * [IEx.Helpers] `IEx.Helpers.recompile/0` will reload modules changed by other operating system processes

#### Mix

  * [mix compile] Ensure warnings from external resources are emitted with `--all-warnings` when files do not change
  * [mix deps.compile] Fix escaping issues when invoking `rebar3` in some cases
  * [mix escript] Fix escript layout and support storing `priv` directories
  * [mix release] Make `.app` files deterministic in releases

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Kernel] Deprecate `unless` in favor of `if`. Use `mix format --migrate` to automate the migration
  * [Macro] `Macro.struct!/2` is deprecated in favor of `Macro.struct_info!/2`

### 4. Hard deprecations

#### EEx

  * [EEx] `<%#` is deprecated in favor of `<%!--` or `<% #`
  * [EEx] `c:EEx.handle_text/2` is deprecated in favor of `c:EEx.handle_text/3`

#### Elixir

  * [Enumerable] Deprecate returning a two-arity function in `Enumerable.slice/1`
  * [List] `List.zip/1` is deprecated in favor of `Enum.zip/1`
  * [Module] Deprecate `Module.eval_quoted/3` in favor of `Code.eval_quoted/3`
  * [Range] Deprecate inferring negative ranges on `Range.new/2`
  * [Tuple] `Tuple.append/2` is deprecated, use `Tuple.insert_at/3` instead

#### Mix

  * [mix cmd] Deprecate `mix cmd --app APP` in favor of `mix do --app APP`
  * [Mix.Tasks.Compile] Deprecate `compilers/0` in favor of `Mix.Task.Compiler.compilers/0`

## v1.17

The CHANGELOG for v1.17 releases can be found [in the v1.17 branch](https://github.com/elixir-lang/elixir/blob/v1.17/CHANGELOG.md).
