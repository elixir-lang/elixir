# Changelog for Elixir v1.16

## Code snippets in diagnostics

Elixir v1.15 introduced a new compiler diagnostic format and the ability to print multiple error diagnostics per compilation (in addition to multiple warnings).

With Elixir v1.16, we also include code snippets in exceptions and diagnostics raised by the compiler. For example, a syntax error now includes a pointer to where the error happened:

```
** (SyntaxError) invalid syntax found on lib/my_app.ex:1:17:
    error: syntax error before: '*'
    │
  1 │ [1, 2, 3, 4, 5, *]
    │                 ^
    │
    └─ lib/my_app.ex:1:17
```

For mismatched delimiters, it now shows both delimiters:

```
** (MismatchedDelimiterError) mismatched delimiter found on lib/my_app.ex:1:18:
    error: unexpected token: )
    │
  1 │ [1, 2, 3, 4, 5, 6)
    │ │                └ mismatched closing delimiter (expected "]")
    │ └ unclosed delimiter
    │
    └─ lib/my_app.ex:1:18
```

Errors and warnings diagnostics also include code snippets. When possible, we will show precise spans, such as on undefined variables:

```
  error: undefined variable "unknown_var"
  │
5 │     a - unknown_var
  │         ^^^^^^^^^^^
  │
  └─ lib/sample.ex:5:9: Sample.foo/1
```

Otherwise the whole line is underlined:

```
error: function names should start with lowercase characters or underscore, invalid name CamelCase
  │
3 │   def CamelCase do
  │   ^^^^^^^^^^^^^^^^
  │
  └─ lib/sample.ex:3
```

A huge thank you to Vinícius Muller for working on the new diagnostics.

## Revamped documentation

Elixir's Getting Started guided has been made part of the Elixir repository and incorporated into ExDoc. This was an opportunity to revisit and unify all official guides and references.

We have also incorporated and extended the work on [Understanding Code Smells in Elixir Functional Language](https://github.com/lucasvegi/Elixir-Code-Smells/blob/main/etc/2023-emse-code-smells-elixir.pdf), by Lucas Vegi and Marco Tulio Valente, from [ASERG/DCC/UFMG](http://aserg.labsoft.dcc.ufmg.br/), into the official document in the form of anti-patterns. The anti-patterns are divided into four categories: code-related, design-related, process-related, and meta-programming. Our goal is to give all developers examples of potential anti-patterns, with context and examples on how to improve their codebases.

Another [ExDoc](https://github.com/elixir-lang/ex_doc) feature we have incorporated in this release is the addition of cheatsheets, starting with [a cheatsheet for the Enum module](https://hexdocs.pm/elixir/main/enum-cheat.html). If you would like to contribute future cheatsheets to Elixir itself, feel free to start a discussion with an issue.

Finally, we have started enriching our documentation with [Mermaid.js](https://mermaid.js.org/) diagrams. You can find examples in the [GenServer](https://hexdocs.pm/elixir/main/GenServer.html) and [Supervisor](https://hexdocs.pm/elixir/main/Supervisor.html) docs.

## v1.16.0-rc.1 (2023-12-12)

### 1. Enhancements

#### Elixir

  * [Code] Add `:emit_warnings` for `Code.string_to_quoted/2`
  * [File] Add `:offset` option to `File.stream!/2`
  * [Kernel] Auto infer size of matched variable in bitstrings
  * [Kernel] Preserve column information when translating typespecs
  * [String] Add `String.replace_invalid/2`

#### Logger

  * [Logger] Add `Logger.levels/0`

#### Mix

  * [mix archive.install] Support `--sparse` option
  * [mix compile.app] Warn if both `:applications` and `:extra_applications` are used
  * [mix compile.elixir] Pass original exception down to diagnostic `:details` when possible
  * [mix deps.clean] Emit a warning instead of crashing when a dependency cannot be removed
  * [mix escript.install] Support `--sparse` option
  * [mix release] Include `include/` directory in releases

### 2. Bug fixes

#### Elixir

  * [Code] Keep quotes for atom keys in formatter
  * [Macro] Address exception on `Macro.to_string/1` for certain ASTs
  * [Module] Make sure file and position information is included in several module warnings (regression)
  * [Path] Lazily evaluate `File.cwd!/0` in `Path.expand/1` and `Path.absname/1`

#### IEx

  * [IEx.Pry] Fix prying functions with only literals in their body

#### Mix

  * [mix archive.install] Restore code paths after archive.install
  * [mix escript.install] Restore code paths after escript.install

## v1.16.0-rc.0 (2023-10-31)

### 1. Enhancements

#### EEx

  * [EEx] Include relative file information in diagnostics

#### Elixir

  * [Code] Automatically include columns in parsing options
  * [Code] Introduce `MismatchedDelimiterError` for handling mismatched delimiter exceptions
  * [Code.Fragment] Handle anonymous calls in fragments
  * [Code.Formatter] Trim trailing whitespace on heredocs with `\r\n`
  * [Kernel] Suggest module names based on suffix and casing errors when the module does not exist in `UndefinedFunctionError`
  * [Kernel.ParallelCompiler] Introduce `Kernel.ParallelCompiler.pmap/2` to compile multiple additional entries in parallel
  * [Kernel.SpecialForms] Warn if `True`/`False`/`Nil` are used as aliases and there is no such alias
  * [Macro] Add `Macro.compile_apply/4`
  * [Module] Add support for `@nifs` annotation from Erlang/OTP 25
  * [Module] Add support for missing `@dialyzer` configuration
  * [String] Update to Unicode 15.1.0
  * [Task] Add `:limit` option to `Task.yield_many/2`

#### Mix

  * [mix] Add `MIX_PROFILE` to profile a list of comma separated tasks
  * [mix compile.elixir] Optimize scenario where there are thousands of files in `lib/` and one of them is changed
  * [mix test] Allow testing multiple file:line at once, such as `mix test test/foo_test.exs:13 test/bar_test.exs:27`

### 2. Bug fixes

#### Elixir

  * [Code.Fragment] Fix crash in `Code.Fragment.surround_context/2` when matching on `->`
  * [IO] Raise when using `IO.binwrite/2` on terminated device (mirroring `IO.write/2`)
  * [Kernel] Do not expand aliases recursively (the alias stored in Macro.Env is already expanded)
  * [Kernel] Ensure `dbg` module is a compile-time dependency
  * [Kernel] Warn when a private function or macro uses `unquote/1` and the function/macro itself is unused
  * [Kernel] Do not define an alias for nested modules starting with `Elixir.` in their definition
  * [Kernel.ParallelCompiler] Consider a module has been defined in `@after_compile` callbacks to avoid deadlocks
  * [Path] Ensure `Path.relative_to/2` returns a relative path when the given argument does not share a common prefix with `cwd`

#### ExUnit

  * [ExUnit] Raise on incorrectly dedented doctests

#### Mix

  * [Mix] Ensure files with duplicate modules are recompiled whenever any of the files change

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [File] Deprecate `File.stream!(file, options, line_or_bytes)` in favor of keeping the options as last argument, as in `File.stream!(file, line_or_bytes, options)`
  * [Kernel.ParallelCompiler] Deprecate `Kernel.ParallelCompiler.async/1` in favor of `Kernel.ParallelCompiler.pmap/2`
  * [Path] Deprecate `Path.safe_relative_to/2` in favor of `Path.safe_relative/2`

### 4. Hard deprecations

#### Elixir

  * [Date] Deprecate inferring a range with negative step, call `Date.range/3` with a negative step instead
  * [Enum] Deprecate passing a range with negative step on `Enum.slice/2`, give `first..last//1` instead
  * [Kernel] `~R/.../` is deprecated in favor of `~r/.../`. This is because `~R/.../` still allowed escape codes, which did not fit the definition of uppercase sigils
  * [String] Deprecate passing a range with negative step on `String.slice/2`, give `first..last//1` instead

#### ExUnit

  * [ExUnit.Formatter] Deprecate `format_time/2`, use `format_times/1` instead

#### Mix

  * [mix compile.leex] Require `:leex` to be added as a compiler to run the `leex` compiler
  * [mix compile.yecc] Require `:yecc` to be added as a compiler to run the `yecc` compiler

## v1.15

The CHANGELOG for v1.15 releases can be found [in the v1.15 branch](https://github.com/elixir-lang/elixir/blob/v1.15/CHANGELOG.md).
