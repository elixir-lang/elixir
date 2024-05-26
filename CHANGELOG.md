# Changelog for Elixir v1.18

This release no longer supports WERL (a graphical user interface for the Erlang terminal on Windows). For a better user experience on Windows terminals, use Erlang/OTP 26+.

## v1.18.0-dev

### 1. Enhancements

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

### 4. Hard deprecations

#### EEx

  * [EEx] `<%#` is deprecated in favor of `<%!--` or `<% #`
  * [EEx] `c:EEx.handle_text/2` is deprecated in favor of `c:EEx.handle_text/3`

#### Elixir

  * [Enumerable] Deprecate returning a two-arity function in `Enumerable.slice/1`
  * [Range] Deprecate inferring negative ranges on `Range.new/2`

#### Mix

  * [mix cmd] Deprecate `mix cmd --app APP` in favor of `mix do --app APP`

## v1.17

The CHANGELOG for v1.17 releases can be found [in the v1.17 branch](https://github.com/elixir-lang/elixir/blob/v1.17/CHANGELOG.md).
