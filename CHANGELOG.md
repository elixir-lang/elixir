# Changelog for Elixir v1.2

v1.2 brings enhancements, bug fixes, performance improvements and more
into Elixir. Elixir v1.2 supports only Erlang 18. Upgrading to Erlang 18
is therefore recommended before upgrading Elixir.

## v1.2.0-dev

### 1. Enhancements

#### Elixir

  * [Application] Add `spec/1` and `spec/2` to retrieve application specification
  * [Base] Optimize encode and decode operations about 10 times
  * [Enum] Use the faster and auto-seeding `:rand` instead of `:random` in `Enum.shuffle/1` and `Enum.random/1` and `Enum.take_random/2`
  * [GenServer] Add `GenServer.stop/1` for reliably shutting servers down
  * [Kernel] Support multiple aliases in `alias`, `import`, `require` and `use`. For example, `alias MyApp.{Foo, Bar, Baz}`
  * [Kernel] Add `struct!/2`. Similar to `struct/2` but raises on invalid keys
  * [Kernel] Warn if `@doc/@typedoc/@moduledoc` attributes are redefined
  * [Kernel] Warn if non-variables are used in `defdelegate/2` (as they have no effect)
  * [Kernel] Mark quoted expressions as generated avoiding false positives on dialyzer
  * [Kernel] Allow variables as map keys on creation `%{key => value}` and on matches `%{^key => value}`
  * [Kernel] Allow the pin operator `^` in `fn` clauses and on the left side of `<-` in `for` comprehensions
  * [Kernel] Introduce `with` as a special form that allows matching on right side parameters
  * [Kernel] Raise when right hand side of `->` does not provide any expression
  * [Kernel] Warn if the Elixir was compiled with a different endianness than the one currently available at runtime
  * [Kernel] Warn if a variable is used after being defined exclusively in a nested context
  * [Kernel] Warn if piping into an expression without parenthesis
  * [Macro] Add `Macro.traverse/4` that performs pre and post-walk at once
  * [Macro] Add `Macro.camelize/1` and `Macro.underscore/1`
  * [Process] Add `Process.get_keys/0`
  * [String] Introduce `String.trim_{prefix,suffix,leading,trailing}/2`. The first two will remove only the first occurrence of the given match in string. The last two will remove all occurrences of the given match
  * [String] Support `String.normalize/2` and `String.equivalent?/2` that perform NFD and NFC normalization
  * [Task] Add `Task.Supervisor.async_nolink/1/3` that spawns a supervised task without linking to the caller process
  * [Task] Introduce `Task.yield_many/2`
  * [Task] Raise an error when a task is queried from a non-owning process (instead of waiting forever)

#### ExUnit

  * [ExUnit] Allow one test to raise multiple errors. The goal is to enable tools in the ecosystem to emit multiple failure reports from the same test
  * [ExUnit] Support `@tag report: [:foo, :bar]` which will include the values for tags `:foo` and `:bar` whenever a test fails

#### IEx

  * [IEx] Display type docs for `t(Module.type)` and `t(Module.type/arity)`
  * [IEx] Add `i/1` helper that prints information about any data type
  * [IEx] Show source code snippet whenever there is a request to pry a given process

#### Logger

  * [Logger] Add file to logger metadata

#### Mix

  * [Mix] Cache and always consolidate protocols
  * [Mix] Add `warn_test_pattern` to `mix test` that will warn on potentially misconfigured test files
  * [Mix] Introduce `MIX_QUIET` environment variable that configures the underlying Mix task to output only error messages
  * [Mix] Validate git options and warn on conflicting ref, branch or tags
  * [Mix] New umbrella applications will now share configuration and build files

### 2. Bug fixes

#### Kernel

  * [Kernel] Change `__ENV__.file` if `@file` is set for the given function
  * [Kernel] Make `Kernel.ParallelRequire` aware of `:warning_as_errors`
  * [Kernel] Improve error message for invalid `do`/`do:`

#### IEx

  * [IEx] Do not start apps on `recompile` helper if `--no-start` was given
  * [IEx] Avoid copying of data when evaluating every expression in IEx

#### Mix

  * [Mix] Always run non-recursive tasks at the umbrella root
  * [Mix] Ensure rebar projects work on directory names that contain non-latin characters
  * [Mix] Ignore directories inside `apps` in umbrellas that do not have a `mix.exs` file
  * [Mix] Ensure Mix can be used with path dependencies where the app name is different than the path basename

#### ExUnit

  * [ExUnit] Include file and line in all compilation errors for doctests

### 3. Soft deprecations (no warnings emitted)

#### Kernel

  * [Dict] `Dict` and `HashDict` are soft deprecated in favor of `Map`
  * [Keyword] `Keyword.size/1` is deprecated in favor of `length/1`
  * [Map] `Map.size/1` is deprecated in favor of `map_size/1`
  * [Set] `Set` and `HashSet` are soft deprecated in favor of `MapSet`

#### Mix

  * [Mix] `Mix.Utils.camelize/1` and `Mix.Utils.underscore/1` are soft deprecated in favor of `Macro.camelize/1` and `Macro.underscore/1`

