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

Prior to this release, Elixir would load modules as soon as they were defined. However, because the Erlang part of code loading happens within a single process (the code server), this would make it a bottleneck, reducing the amount of parallelization, especially on large projects.

This release makes it so modules are loaded lazily. This reduces the pressure on the code server, making compilation up to 2x faster for large projects, and also reduces the overall amount of work done during compilation.

Implementation wise, [the parallel compiler already acts as a mechanism to resolve modules during compilation](https://elixir-lang.org/blog/2012/04/24/a-peek-inside-elixir-s-parallel-compiler/), so we built on that. By making sure the compiler controls both module compilation and module loading, it can also better guarantee deterministic builds.

The only potential regression in this approach happens if you have a module, which is used at compile time and defines an `@on_load` callback (typically used for [NIFs](https://www.erlang.org/doc/system/nif.html)) that invokes another modules within the same project. For example:

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

### Parallel compilation of dependencies

This release introduces a variable called `MIX_OS_DEPS_COMPILE_PARTITION_COUNT`, which instructs `mix deps.compile` to compile dependencies in parallel.

While fetching dependencies and compiling individual Elixir dependencies already happened in parallel, there were pathological cases where performance would be left on the table, such as compiling dependencies with native code or dependencies where one or two large file would take over most of the compilation time.

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

This allows for more information to be shown at different nesting levels, which is useful for complex data structures. But it led to some pathological cases where the `limit` option had little effect on actually filtering the amount of data shown. The new implementation decouples the limit handling from depth, decreasing it as it goes. Therefore, the list above with the same limit in Elixir v1.19 is now printed as:

```elixir
[
  [1, 2, 3],
  ...
]
```

The outer list is the first element, the first nested list is the second, followed by three numbers, reaching the limit. This gives developers more precise control over pretty printing.

Given this may reduce the amount of data printed by default, the default limit has also been increased from 50 to 100. We may further increase it in upcoming releases based on community feedback.

## OpenChain certification

Elixir v1.19 is also our first release following OpenChain compliance, [as previously announced](https://elixir-lang.org/blog/2025/02/26/elixir-openchain-certification/). In a nutshell:

  * Elixir releases now include a Source SBoM in CycloneDX 1.6 or later and SPDX 2.3 or later formats.
  * Each release is attested along with the Source SBoM.

These additions offer greater transparency into the components and licenses of each release, supporting more rigorous supply chain requirements.

This work was performed by Jonatan Männchen and sponsored by the Erlang Ecosystem Foundation.

## v1.19.0-rc.1

### 1. Enhancements

#### Elixir

  * [Kernel] Raise when U+2028 and U+2029 characters are present in comments and strings to avoid line spoofing attacks
  * [Macro] Add `__escape__/1` callback so structs can escape references and other runtime data types in `Macro.escape/1`
  * [OptionParser] Support the `:regex` type
  * [OptionParser] Enhance parsing error to display available options

#### Mix

  * [mix format] Add options to mix format to allow excluding of files
  * [mix test] Add `--name-pattern` option to `mix test`
  * [Mix.install/2] Support the `:compilers` option

### 2. Bug fixes

#### Elixir

  * [Code] Return error on invalid unicode sequences in `Code.string_to_quoted/2` instead of raising
  * [Kernel] Properly increment metadata newline when `?` is followed ny a literal newline character

#### ExUnit

  * [ExUnit.Assertions] Fix order in ExUnit results when listing pinned variables

## v1.19.0-rc.0 (2025-06-09)

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
  * [Kernel] Allow controlling which applications are used during inference
  * [Kernel] Support `min/2` and `max/2` as guards
  * [Kernel.ParallelCompiler] Add `each_long_verification_threshold` which invokes a callback when type checking a module takes too long
  * [Kernel.ParallelCompiler] Include lines in `== Compilation error in file ... ==` slogans
  * [Macro] Print debugging results from `Macro.dbg/3` as they happen, instead of once at the end
  * [Module] Do not automatically load modules after their compilation, guaranteeing a more consistent compile time experience and drastically improving compilation times
  * [Protocol] Type checking of protocols dispatch and implementations
  * [Regex] Add `Regex.to_embed/2` which returns an embeddable representation of regex in another regex
  * [String] Add `String.count/2` to count occurrences of a pattern

#### ExUnit

  * [ExUnit.CaptureLog] Parallelize log dispatch when multiple processes are capturing log
  * [ExUnit.Case] Add `:test_group` to the test context
  * [ExUnit.Doctest] Support ellipsis in doctest exceptions to match the remaining of the exception
  * [ExUnit.Doctest] Add `:inspect_opts` option for doctest

#### IEx

  * [IEx] Support multi-line prompts (due to this feature, `:continuation_prompt` and `:alive_continuation_prompt` are no longer supported as IEx configuration)
  * [IEx.Autocomplete] Functions annotated with `@doc group: "Name"` metadata will appear within their own groups in autocompletion

#### Mix

  * [mix] Add support for `MIX_PROFILE_FLAGS` to configure `MIX_PROFILE`
  * [mix compile] Debug the compiler and type checker PID when `MIX_DEBUG=1` and compilation/verification thresholds are met
  * [mix compile] Add `Mix.Tasks.Compiler.reenable/1`
  * [mix deps.compile] Support `MIX_OS_DEPS_COMPILE_PARTITION_COUNT` for compiling deps concurrently across multiple operating system processes
  * [mix help] Add `mix help Mod`, `mix help :mod`, `mix help Mod.fun` and `mix help Mod.fun/arity`
  * [mix test] Allow to distinguish the exit status between warnings as errors and test failures
  * [mix xref graph] Add support for `--format json`
  * [mix xref graph] Emit a warning if `--source` is part of a cycle
  * [M ix.Task.Compiler] Add `Mix.Task.Compiler.run/2`

### 2. Bug fixes

#### Elixir

  * [DateTime] Do not truncate microseconds regardless of precision in `DateTime.diff/3`
  * [File] Properly handle permissions errors cascading from parent in `File.mkdir_p/1`
  * [Kernel] `not_a_map.key` now raises `BadMapError` for consistency with other map operations
  * [Protocol] `defstruct/1` and `defexception/1` are now disabled inside `defprotocol` as to not allow defining structs/exceptions alongside a protocol
  * [Regex] Fix `Regex.split/2` returning too many results when the chunk being split on was empty (which can happen when using features such as `/K`)
  * [Stream] Ensure `Stream.transform/5` respects suspend command when its inner stream halts
  * [URI] Several fixes to `URI.merge/2` related to trailing slashes, trailing dots, and hostless base URIs

#### Mix

  * [mix cmd] Preserve argument quoting in subcommands
  * [mix format] Ensure the formatter does not go over the specified limit in certain corner cases
  * [mix release] Fix `RELEASE_SYS_CONFIG` for Windows 11
  * [mix test] Preserve files with no longer filter on `mix test`
  * [mix xref graph] Provide more consistent output by considering strong connected components only when computing graphs

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Inspect.Algebra] `next_break_fits` is deprecated in favor of `optimistic`/`pessimistic` groups
  * [Node] `Node.start/2-3` is deprecated in favor of `Node.start/2` with a keyword list

#### Mix

  * [mix compile] `--no-protocol-consolidation` is deprecated in favor of `--no-consolidate-protocols` for consistency with `mix.exs` configuration
  * [mix compile.protocols] Protocol consolidation is now part of `compile.elixir` and has no effect

### 4. Hard deprecations

#### Elixir

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
