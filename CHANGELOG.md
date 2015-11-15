# Changelog for Elixir v1.2

v1.2 brings enhancements, bug fixes, performance improvements and more
into Elixir. Elixir v1.2 supports only Erlang 18. Upgrading to Erlang 18
is therefore recommended before upgrading Elixir.

## v1.2.0-dev

### 1. Enhancements

#### Elixir

  * [Base] Optimize encode and decode operations about 10 times
  * [Enum] Use the faster and auto-seeding `:rand` instead of `:random` in `Enum.shuffle/1` and `Enum.random/1` and `Enum.take_random/2`
  * [GenServer] Add `GenServer.stop/1` for reliably shutting servers down
  * [Kernel] Support multiple aliases in `alias`, `import`, `require` and `use`. For example, `alias MyApp.{Foo, Bar, Baz}`
  * [Kernel] Add `struct!/2`. Similar to `struct/2` but raises on invalid keys
  * [Kernel] Warn if `@doc/@typedoc/@moduledoc` attributes are redefined
  * [Kernel] Mark quoted expressions as generated avoiding false positives on dialyzer
  * [Kernel] Allow variables as map keys on creation `%{key => value}` and on matches `%{^key => value}`
  * [Kernel] Allow the pin operator `^` in `fn` clauses and on the left side of `<-` in `for` comprehensions
  * [Kernel] Introduce `with` as a special form that allows matching on right side parameters
  * [Kernel] Raise when right hand side of `->` does not provide any expression
  * [Kernel] Warn if the Elixir was compiled with a different endianness than the one currently available at runtime
  * [Macro] Add `Macro.traverse/4` that performs pre and post-walk at once
  * [Macro] Add `Macro.camelize/1` and `Macro.underscore/1`
  * [Process] Add `Process.get_keys/0`
  * [String] Introduce `String.trim_{prefix,suffix,leading,trailing}/2`. The first two will remove only the first occurrence of the given match in string. The last two will remove all occurrences of the given match
  * [String] Support `String.normalize/2` and `String.equivalent?/2` that perform NFD and NFC normalization
  * [Task] Add `Task.Supervisor.async_nolink/1/3` that spawns a supervised task without linking to the caller process
  * [Task] Raise an error when a task is queried from a non-owning process (instead of waiting forever)

#### IEx

  * [IEx] Display type docs for `t(Module.type)` and `t(Module.type/arity)`

#### Logger

  * [Logger] Add file to logger metadata

#### Mix

  * [Mix] Cache and always consolidate protocols
  * [Mix] Add `warn_test_pattern` to `mix test` that will warn on potentially misconfigured test files
  * [Mix] Introduce `MIX_QUIET` environment variable that configures the underlying Mix task to output only error messages
  * [Mix] Validate git options and warn on conflicting ref, branch or tags

### 2. Bug fixes

#### IEx

  * [IEx] Do not start apps on `recompile` helper if `--no-start` was given

#### Mix

  * [Mix] Always run non-recursive tasks at the umbrella root
  * [Mix] Ensure rebar projects work on directory names that contain non-latin characters
  * [Mix] Ignore directories inside `apps` in umbrellas that do not have a `mix.exs` file

### 3. Soft deprecations (no warnings emitted)

#### Mix

  * [Mix] `Mix.Utils.camelize/1` and `Mix.Utils.underscore/1` are soft deprecated in favor of `Macro.camelize/1` and `Macro.underscore/1`

### 4. Deprecations
