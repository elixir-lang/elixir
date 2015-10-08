# Changelog for Elixir v1.2

v1.2 brings enhancements, bug fixes, performance improvements and more
into Elixir. Elixir v1.2 supports only Erlang 18. Upgrading to Erlang 18
is therefore recommended before upgrading Elixir.

## v1.2.0-dev

### 1. Enhancements

#### Elixir

* [Kernel] Support multiple aliases in `alias`, `import`, `require` and `use`. For example, `alias MyApp.{Foo, Bar, Baz}`
* [Kernel] Add `struct!/2`. Similar to `struct/2` but raises on invalid keys
* [Macro] Add `Macro.traverse/4` that performs pre and post-walk at once
* [String] Support `String.normalize/2` and `String.equivalent?/2` that perform NFD and NFC normalization

#### EEx

#### ExUnit

#### IEx

* [IEx] Display type docs for `t(Module.type)` and `t(Module.type/arity)`

#### Logger

#### Mix

* [Mix] Cache and always consolidate protocols

### 2. Bug fixes

#### Mix

* [Mix] Always run non-recursive tasks at the umbrella root

### 3. Soft deprecations (no warnings emitted)

### 4. Deprecations
