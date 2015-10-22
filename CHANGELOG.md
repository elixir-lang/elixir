# Changelog for Elixir v1.2

v1.2 brings enhancements, bug fixes, performance improvements and more
into Elixir. Elixir v1.2 supports only Erlang 18. Upgrading to Erlang 18
is therefore recommended before upgrading Elixir.

## v1.2.0-dev

### 1. Enhancements

#### Elixir

  * [Base] Optimize encode and decode operations about 10 times
  * [Enum] Use the faster and auto-seeding `:rand` instead of `:random` in `Enum.shuffle/1` and `Enum.random/1` and `Enum.take_random/2`
  * [Kernel] Support multiple aliases in `alias`, `import`, `require` and `use`. For example, `alias MyApp.{Foo, Bar, Baz}`
  * [Kernel] Add `struct!/2`. Similar to `struct/2` but raises on invalid keys
  * [Kernel] Warn if `@doc/@typedoc/@moduledoc` attributes are redefined
  * [Kernel] Mark quoted expressions as generated avoiding false positives on dialyzer
  * [Kernel] Allow variables as map keys on creation `%{key => value}` and on matches `%{^key => value}`
  * [Kernel] Allow the pin operator `^` in `fn` clauses and on the left side of `<-` in `for` comprehensions
  * [Macro] Add `Macro.traverse/4` that performs pre and post-walk at once
  * [Macro] Add `Macro.camelize/1` and `Macro.underscore/1`
  * [Process] Add `Process.get_keys/0`
  * [String] Introduce `String.trim_{prefix,suffix,leading,trailing}/2`. The first two will remove only the first occurrence of the given match in string. The last two will remove all occurrences of the given match
  * [String] Support `String.normalize/2` and `String.equivalent?/2` that perform NFD and NFC normalization
  * [Task] Add `Task.Supervisor.async_nolink/1/3` that spawns a supervised task without linking to the caller process

#### EEx

#### ExUnit

#### IEx

  * [IEx] Display type docs for `t(Module.type)` and `t(Module.type/arity)`
  * [IEx] Display a list of all modules with `m/0` or module specific information with `m/1`

#### Logger

#### Mix

  * [Mix] Cache and always consolidate protocols
  * [Mix] Add `warn_test_pattern` to `mix test` that will warn on potentially misconfigured test files

### 2. Bug fixes

#### Mix

  * [Mix] Always run non-recursive tasks at the umbrella root

### 3. Soft deprecations (no warnings emitted)

#### Mix

  * [Mix] `Mix.Utils.camelize/1` and `Mix.Utils.underscore/1` are soft deprecated in favor of `Macro.camelize/1` and `Macro.underscore/1`

### 4. Deprecations
