<!--
  SPDX-License-Identifier: Apache-2.0
  SPDX-FileCopyrightText: 2021 The Elixir Team
  SPDX-FileCopyrightText: 2012 Plataformatec
-->

# Changelog for Elixir v1.19

## Type system improvements

### Type checking of protocol dispatch and implementations

This release also adds type checking when dispatching and implementing protocols.

For example, string interpolation in Elixir uses the `String.Chars` protocol. If you pass a value that does not implement said protocol, Elixir will now emit a warning accordingly.

Here is an example passing a range, which cannot be converted into a string, to an interpolation:

```elixir
defmodule Example do
  def my_code(first..last//step = range) do
    "hello #{range}"
  end
end
```

the above emits the following warnings:

```
warning: incompatible value given to string interpolation:

    data

it has type:

    %Range{first: term(), last: term(), step: term()}

but expected a type that implements the String.Chars protocol, it must be one of:

    dynamic(
      %Date{} or %DateTime{} or %NaiveDateTime{} or %Time{} or %URI{} or %Version{} or
        %Version.Requirement{}
    ) or atom() or binary() or float() or integer() or list(term())
```

Warnings are also emitted if you pass a data type that does not implement the `Enumerable` protocol as a generator to for-comprehensions:

```elixir
defmodule Example do
  def my_code(%Date{} = date) do
    for(x <- date, do: x)
  end
end
```

will emit:

```
warning: incompatible value given to for-comprehension:

    x <- date

it has type:

    %Date{year: term(), month: term(), day: term(), calendar: term()}

but expected a type that implements the Enumerable protocol, it must be one of:

    dynamic(
      %Date.Range{} or %File.Stream{} or %GenEvent.Stream{} or %HashDict{} or %HashSet{} or
        %IO.Stream{} or %MapSet{} or %Range{} or %Stream{}
    ) or fun() or list(term()) or non_struct_map()
```

### Type checking and inference of anonymous functions

Elixir v1.19 can now type infer and type check anonymous functions. Here is a trivial example:

```elixir
defmodule Example do
  def run do
    fun = fn %{} -> :map end
    fun.("hello")
  end
end
```

The example above has an obvious typing violation, as the anonymous function expects a map but a string is given. With Elixir v1.19, the following warning is now printed:

```
    warning: incompatible types given on function application:

        fun.("hello")

    given types:

        binary()

    but function has type:

        (dynamic(map()) -> :map)

    typing violation found at:
    │
  6 │     fun.("hello")
    │        ~
    │
    └─ mod.exs:6:8: Example.run/0
```

Function captures, such as `&String.to_integer/1`, will also propagate the type as of Elixir v1.19, arising more opportunity for Elixir's type system to catch bugs in our programs.

### Acknowledgements

The type system was made possible thanks to a partnership between [CNRS](https://www.cnrs.fr/) and [Remote](https://remote.com/). The development work is currently sponsored by [Fresha](https://www.fresha.com/), [Starfish*](https://starfish.team/), and [Dashbit](https://dashbit.co/).

## Faster compile times in large projects

This release includes two compiler improvements that can lead up to 4x faster builds in large codebases.

While Elixir has always compiled the given files in project or a dependency in parallel, the compiler would sometimes be unable to use all of the machine resources efficiently. This release addresses two common limitations, delivering performance improvements that scale with codebase size and available CPU cores.

### Code loading bottlenecks

Prior to this release, Elixir would load modules as soon as they were defined. However, because the Erlang part of code loading happens within a single process (the code server), this would make it a bottleneck, reducing parallelization, especially on large projects.

This release makes it so modules are loaded lazily. This reduces the pressure on the code server and the amount of work during compilation, with reports of more than two times faster compilation for large projects. The benefits depend on the codebase size and the number of CPU cores available.

Implementation wise, [the parallel compiler already acts as a mechanism to resolve modules during compilation](https://elixir-lang.org/blog/2012/04/24/a-peek-inside-elixir-s-parallel-compiler/), so we built on that. By making sure the compiler controls both module compilation and module loading, it can also better guarantee deterministic builds.

There are two potential regressions with this approach. The first one happens if you spawn processes during compilation which invoke other modules defined within the same project. For example:

```elixir
defmodule MyLib.SomeModule do
  list = [...]

  Task.async_stream(list, fn item ->
    MyLib.SomeOtherModule.do_something(item)
  end)
end
```

Because the spawned process is not visible to the compiler, it won't be able to load `MyLib.SomeOtherModule`. You have two options, either use `Kernel.ParallelCompiler.pmap/2` or explicitly call `Code.ensure_compiled!(MyLib.SomeOtherModule)` before spawning the process that uses said module.

The second one is related to `@on_load` callbacks (typically used for [NIFs](https://www.erlang.org/doc/system/nif.html)) that invoke other modules defined within the same project. For example:

```elixir
defmodule MyLib.SomeModule do
  @on_load :init

  def init do
    MyLib.AnotherModule.do_something()
  end

  def something_else do
    ...
  end
end

MyLib.SomeModule.something_else()
```

The reason this fails is because `@on_load` callbacks are invoked within the code server and therefore they have limited ability to load additional modules. It is generally advisable to limit invocation of external modules during `@on_load` callbacks but, in case it is strictly necessary, you can set `@compile {:autoload, true}` in the invoked module to address this issue in a forward and backwards compatible manner.

Both snippets above could actually lead to non-deterministic compilation failures in the past, and as a result of these changes, compiling these cases are now deterministic.

### Parallel compilation of dependencies

This release introduces a variable called `MIX_OS_DEPS_COMPILE_PARTITION_COUNT`, which instructs `mix deps.compile` to compile dependencies in parallel.

While fetching dependencies and compiling individual Elixir dependencies already happened in parallel, as outlined in the previous section, there were pathological cases where performance gains would be left on the table, such as when compiling dependencies with native code or dependencies where one or two large files would take most of the compilation time.

By setting `MIX_OS_DEPS_COMPILE_PARTITION_COUNT` to a number greater than 1, Mix will now compile multiple dependencies at the same time, using separate OS processes. Empirical testing shows that setting it to half of the number of cores on your machine is enough to maximize resource usage. The exact speed up will depend on the number of dependencies and the number of machine cores, although some reports mention up to 4x faster compilation times. If you plan to enable it on CI or build servers, keep in mind it will most likely have a direct impact on memory usage too.

## Improved pretty printing algorithm

Elixir v1.19 ships with a new pretty printing implementation that tracks limits as a whole, instead of per depth. Previous versions would track limits per depth. For example, if you had a list of lists of 4 elements and a limit of 5, it would be pretty printed as follows:

```elixir
[
  [1, 2, 3],
  [1, 2, ...],
  [1, ...],
  [...],
  ...
]
```

This allows for more information to be shown at different nesting levels, which is useful for complex data structures. But it led to some pathological cases where the `limit` option had little effect on filtering the amount of data shown. The new implementation decouples the limit handling from depth, decreasing it as it goes. Therefore, the list above with the same limit in Elixir v1.19 is now printed as:

```elixir
[
  [1, 2, 3],
  ...
]
```

The outer list is the first element, the first nested list is the second, followed by three numbers, reaching the limit. This gives developers more precise control over pretty printing.

Given this may reduce the amount of data printed by default, the default limit has also been increased from 50 to 100. We may further increase it in upcoming releases based on community feedback.

## Erlang/OTP 28 support

Elixir v1.19 officially supports Erlang/OTP 28.1+ and later. In order to support the new Erlang/OTP 28 representation for regular expressions, structs can now control how they are escaped into abstract syntax trees by defining a `__escape__/1` callback.

On the other hand, the new representation for regular expressions implies they can no longer be used as default values for struct fields. Instead of this:

```elixir
defmodule Foo do
  defstruct regex: ~r/foo/
end
```

You must do this:

```elixir
defmodule Foo do
  defstruct [:regex]

  def new do
    %Foo{regex: ~r/foo/}
  end
end
```

## OpenChain certification

Elixir v1.19 is also our first release following OpenChain compliance, [as previously announced](https://elixir-lang.org/blog/2025/02/26/elixir-openchain-certification/). In a nutshell:

  * Elixir releases now include a Source SBoM in CycloneDX 1.6 or later and SPDX 2.3 or later formats.
  * Each release is attested along with the Source SBoM.

These additions offer greater transparency into the components and licenses of each release, supporting more rigorous supply chain requirements.

This work was performed by [Jonatan Männchen](https://maennchen.dev) and sponsored by the [Erlang Ecosystem Foundation](https://erlef.org).

## v1.19.5 (2026-01-09)

### 1. Enhancements

#### Elixir

  * [Protocol] Optimize protocol consolidation to no longer load structs

### 2. Bug fixes

#### Elixir

  * [Kernel] Fix unnecessary recompilation when `dbg_callback` is modified at runtime
  * [Kernel] Fix parser crash on missing parentheses on expression following operator `not in`
  * [Kernel] Support fetching abstract code for modules compiled with Elixir v1.14 and earlier
  * [Protocol] Ensure protocol consolidation no longer stores outdated struct types. As a consequence, protocols types only track struct names at the moment
  * [Stream] Revert optimization which caused nested streams in `Stream.flat_map/2` to crash

#### IEx

  * [IEx] Fix usage of `#iex:break` as part of multi-line prompts

#### Logger

  * [Logger.Backends] Do not crash on invalid metadata

## v1.19.4 (2025-11-27)

### 1. Enhancements

#### Mix

  * [mix xref] Add `--min-cycle-label` to help projects adapt to the more precise `mix xref graph` reports in Elixir v1.19. In previous versions, Elixir would break a large compilation cycle into several smaller ones, and therefore developers would check for `--min-cycle-size` on CI. However, the issue is not the size of the cycle (it has no implication in the amount of compiled files), but how many compile-time dependencies (aka compile labels) in a cycle. The new option allows developers to filter on the label parameter

### 2. Bug fixes

#### Elixir

  * [File] Ensure `File.cp_r/3` reports non-existing destination properly (instead of source)

#### ExUnit

  * [ExUnit] Fix formatter crash when diffing takes too long
  * [ExUnit] Ensure parallel matches in `assert` propagate type information

#### Logger

  * [Logger] Fix regression where formatter would crash when given chardata (the crash would happen when logging non-ASCII characters)

#### Mix

  * [mix help] Ensure `app:APP` works when the project or its dependencies were not yet compiled
  * [mix escript.build] Ensure the `hex` application can be included in escripts

## v1.19.3 (2025-11-13)

### 1. Enhancements

#### Elixir

  * [Kernel] Support /E modifier for regular expressions in config files

#### Mix

  * [mix compile] Allow forcing specific compilers, such as `--force-elixir`, `--force-app`, etc
  * [mix help app:APP] Support showing helps for apps in Elixir and Erlang standard libraries

### 2. Bug fixes

#### Elixir

  * [IO] Fix dialyzer warning on `IO.inspect :label`

#### ExUnit

  * [ExUnit.Case] Fix crash when formatting errors caused by a linked/trapped exit during `setup_all`

#### Mix

  * [mix compile.app] Ensure functions in the format `&Mod.fun/arity` can be written to .app files
  * [mix compile.app] Ensure strings with Unicode characters can be written to .app files

## v1.19.2 (2025-11-02)

### 1. Enhancements

#### Elixir

  * [Kernel] Measure and optimize writing of `.beam` files in the compiler
  * [Kernel] Optimize rare scenarios where type checking took too long

#### Mix

  * [mix compile] Add flag `--no-check-cwd` to skip compiler check to aid debugging

### 2. Bug fixes

#### Elixir

  * [IO] Fix dialyzer warning on `IO.inspect :label`
  * [Kernel] Ensure we warn on deprecated `~~~` unary operator

#### Logger

  * [Logger] Reset ansi escapes before newlines in Logger

#### Mix

  * [mix compile] Warn if `elixirc_paths` is not a list of string paths
  * [mix compile] Address regression where umbrella children were compiled too early and without respecting compilation flags
  * [mix deps.compile] Improve reliability of `MIX_OS_DEPS_COMPILE_PARTITION_COUNT` across `mix escript.install`, `mix archive.install`, and others

## v1.19.1 (2025-10-20)

### 1. Bug fixes

#### EEx

  * [EEx] Address Dialyzer warnings when invoking `EEx.compile_string`

#### Elixir

  * [Kernel] Optimize how types are computed for pretty printing
  * [Kernel] Optimize how differences are computed in the type system
  * [Macro] Do not escape options given to `dbg/2`
  * [Protocol] Improve protocol violation warnings

#### Mix

  * [mix compile] Do not attempt to touch deleted files when compilation fails and then resumed with missing files
  * [mix deps.compile] Do not spawn partitions when all dependencies are local and already compiled

## v1.19.0 (2025-10-16)

### 1. Enhancements

#### Elixir

  * [Access] Add `Access.values/0` for traversing maps and keyword lists values
  * [Base] Add functions to verify if an encoding is valid, such as `valid16?`, `valid64?`, and so forth
  * [Calendar] Support 2-arity options for `Calendar.strftime/3` which receives the whole data type
  * [Code] Add `:migrate_call_parens_on_pipe` formatter option
  * [Code] Add `:indentation` option to `Code.string_to_quoted/2`
  * [Code.Fragment] Preserve more block content around cursor in `container_cursor_to_quoted`
  * [Code.Fragment] Add `:block_keyword_or_binary_operator` to `Code.Fragment` for more precise suggestions after operators and closing terminators
  * [Code.Fragment] Add `Code.Fragment.lines/1`
  * [Enum] Provide more information on `Enum.OutOfBoundsError`
  * [Inspect] Allow `optional: :all` when deriving Inspect
  * [Inspect.Algebra] Add optimistic/pessimistic groups as a simplified implementation of `next_break_fits`
  * [IO.ANSI] Add ANSI codes to turn off conceal and crossed_out
  * [Kernel] Raise when U+2028 and U+2029 characters are present in comments and strings to avoid line spoofing attacks
  * [Kernel] Include the line for the previous clause in errors/warnings related to conflicts between defaults on function definitions
  * [Kernel] Support `min/2` and `max/2` as guards
  * [Kernel.ParallelCompiler] Add `each_long_verification_threshold` which invokes a callback when type checking a module takes too long
  * [Kernel.ParallelCompiler] Include lines in `== Compilation error in file ... ==` slogans
  * [Macro] Print debugging results from `Macro.dbg/3` as they happen, instead of once at the end
  * [Macro] Add `__escape__/1` callback so structs can escape references and other runtime data types in `Macro.escape/1`
  * [Module] Do not automatically load modules after their compilation, guaranteeing a more consistent compile time experience and drastically improving compilation times
  * [OptionParser] Support the `:regex` type
  * [OptionParser] Enhance parsing error to display available options
  * [Protocol] Type checking of protocols dispatch and implementations
  * [Regex] Add `Regex.to_embed/2` which returns an embeddable representation of regex in another regex
  * [Regex] Raise error message when regexes are used as default values in struct fields for compatibility with Erlang/OTP 28
  * [Registry] Add key-based partitioning of duplicate registries
  * [String] Add `String.count/2` to count occurrences of a pattern
  * [String] Update to Unicode 17.0.0

#### ExUnit

  * [ExUnit] Set a process label for each test
  * [ExUnit.CaptureLog] Parallelize log dispatch when multiple processes are capturing log
  * [ExUnit.Case] Add `:test_group` to the test context
  * [ExUnit.Doctest] Support ellipsis in doctest exceptions to match the remaining of the exception
  * [ExUnit.Doctest] Add `:inspect_opts` option for doctest

#### IEx

  * [IEx] Support multi-line prompts (due to this feature, `:continuation_prompt` and `:alive_continuation_prompt` are no longer supported as IEx configuration)
  * [IEx.Autocomplete] Functions annotated with `@doc group: "Name"` metadata will appear within their own groups in autocompletion

#### Logger

  * [Logger] Accept any enumerable in `Logger.metadata/1`

#### Mix

  * [mix] Add support for `MIX_PROFILE_FLAGS` to configure `MIX_PROFILE`
  * [mix compile] Debug the compiler and type checker PID when `MIX_DEBUG=1` and compilation/verification thresholds are met
  * [mix compile] Add `Mix.Tasks.Compiler.reenable/1`
  * [mix deps.compile] Support `MIX_OS_DEPS_COMPILE_PARTITION_COUNT` for compiling deps concurrently across multiple operating system processes
  * [mix help] Add `mix help Mod`, `mix help :mod`, `mix help Mod.fun`, `mix help Mod.fun/arity`, and `mix help app:package`
  * [mix format] Add options to mix format to allow excluding of files
  * [mix test] Add `--name-pattern` option to `mix test`
  * [mix test] Allow to distinguish the exit status between warnings as errors and test failures
  * [mix xref graph] Add support for `--format json`
  * [mix xref graph] Emit a warning if `--source` is part of a cycle
  * [Mix] Support the `:compilers` option
  * [Mix.Task.Compiler] Add `Mix.Task.Compiler.run/2`

### 2. Bug fixes

#### Elixir

  * [Code] Return error on invalid unicode sequences in `Code.string_to_quoted/2` instead of raising
  * [Code] Properly handle column annotation for `in` in `not in` expressions
  * [DateTime] Do not truncate microseconds regardless of precision in `DateTime.diff/3`
  * [Enum] Fix infinite loop on `Enum.take/2` with negative index on empty enumerable
  * [File] Properly handle permissions errors cascading from parent in `File.mkdir_p/1`
  * [Inspect] Inspect ill-formed structs as maps
  * [Kernel] Properly increment metadata newline when `?` is followed by a literal newline character
  * [Kernel] `not_a_map.key` now raises `BadMapError` for consistency with other map operations
  * [Protocol] `defstruct/1` and `defexception/1` are now disabled inside `defprotocol` as to not allow defining structs/exceptions alongside a protocol
  * [Regex] Fix `Regex.split/2` returning too many results when the chunk being split on was empty (which can happen when using features such as `/K`)
  * [Stream] Ensure `Stream.transform/5` respects suspend command when its inner stream halts
  * [URI] Several fixes to `URI.merge/2` related to trailing slashes, trailing dots, and hostless base URIs

#### ExUnit

  * [ExUnit.Assertions] Fix order of pinned variables in failure reports
  * [ExUnit.Assertions] Raise if attempting to raise an assertion error with invalid message (not a binary)
  * [ExUnit.Case] Do not crash on empty test unit groups

#### IEx

  * [IEx] Abort pipelines when there is an error in any step along the way

#### Mix

  * [mix cmd] Preserve argument quoting in subcommands by no longer performing shell expansion. To revert to the previous behaviour, pass `--shell` before the command name
  * [mix compile] Fix bug where reverting changes to an external resource (such as HEEx template) after a compilation error would make it so the source module would not be compiled
  * [mix compile] Avoid failures when locking compilation across different users
  * [mix compile] Fix race condition when renaming files used by the compilation lock
  * [mix format] Ensure the formatter does not go over the specified limit in certain corner cases
  * [mix release] Fix `RELEASE_SYS_CONFIG` for Windows 11
  * [mix test] Ensure modules are preloaded in `mix test --slowest-modules=N`
  * [mix xref graph] Provide more consistent output by considering strong connected components when computing graphs. This means that, if you were using `mix xref graph --format cycles`, you will now get fewer but larger cycles, as cycle between `A` and `B` and another between `A` and `C` is now considered a single cycle between `A`, `B`, and `C`. Note the cycles themselves are not problematic, unless they have compile-time dependencies in them

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Inspect.Algebra] `next_break_fits` is deprecated in favor of `optimistic`/`pessimistic` groups
  * [Node] `Node.start/2-3` is deprecated in favor of `Node.start/2` with a keyword list

#### Mix

  * [mix compile] `--no-protocol-consolidation` is deprecated in favor of `--no-consolidate-protocols` for consistency with `mix.exs` configuration
  * [mix compile.protocols] Protocol consolidation is now part of `compile.elixir` and the task itself has no effect

### 4. Hard deprecations

#### Elixir

  * [Code] Warn if line-break characters outside of `\r` and `\r\n` are found in strings according to UX#55. This warning will be fast-tracked into an error for security reasons in Elixir v1.20, following a similar rule to bidirectional control characters. They will already raise if found in comments
  * [Code] The `on_undefined_variable: :warn` is deprecated. Relying on undefined variables becoming function calls will not be supported in the future
  * [File] Passing a callback as third argument to `File.cp/3` is deprecated, pass it as a `on_conflict: callback` option instead
  * [File] Passing a callback as third argument to `File.cp_r/3` is deprecated, pass it as a `on_conflict: callback` option instead
  * [Kernel] The struct update syntax, such as `%URI{uri | path: "/foo/bar"}`, now requires the given variable (or expression) to explicitly pattern match on the struct before it can be updated. This is because, thanks to the type system, pattern matching on structs can find more errors, more reliably, and we want to promote its usage. Once pattern matching is added, you may optionally convert the struct update syntax into the map update syntax `%{uri | path: "/foo/bar"}` with no less of typing guarantees
  * [Kernel.ParallelCompiler] Passing `return_diagnostics: true` as an option is required on `compile`, `compile_to_path` and `require`

#### Logger

  * [Logger] The `:backends` configuration is deprecated, either set the `:default_handler` to false or start backends in your application start callback

#### Mix

  * [mix] The `:default_task`, `:preferred_cli_env`, and `:preferred_cli_target` configuration inside `def project` in your `mix.exs` has been deprecated in favor of `:default_task`, `:preferred_envs` and `:preferred_targets` inside the `def cli` function
  * [mix do] Using commas as task separator in `mix do` (such as `mix do foo, bar`) is deprecated, use `+` instead (as in `mix do foo + bar`)

## v1.18

The CHANGELOG for v1.18 releases can be found [in the v1.18 branch](https://github.com/elixir-lang/elixir/blob/v1.18/CHANGELOG.md).
