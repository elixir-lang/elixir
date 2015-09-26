# Changelog for Elixir v1.2

v1.2 brings enhancements, bug fixes, performance improvements and more
into Elixir. Elixir v1.2 supports only Erlang 18. Upgrading to Erlang 18
is therefore recommended before upgrading Elixir.

## v1.2.0-dev

### 1. Enhancements

#### Elixir

* [Kernel] Support multiple aliases in `alias`, `import`, `require` and `use`. For example, `alias MyApp.{Foo, Bar, Baz}`
* [Kernel] Add `struct!/2`. Similar to `struct/2` but raises on invalid keys
* [String] Support `String.normalize/2` and `String.equivalent?/2` that perform NFD normalization and equivalent

#### EEx

#### ExUnit

#### IEx

#### Logger

#### Mix

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

### 4. Deprecations
