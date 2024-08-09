# Changelog for Elixir v1.18

This release no longer supports WERL (a graphical user interface for the Erlang terminal on Windows). For a better user experience on Windows terminals, use Erlang/OTP 26+.

## v1.18.0-dev

### 1. Enhancements

#### Elixir

  * [CLI] Add experimental PowerShell scripts for `elixir`, `elixirc`, and `mix` on Windows. Those provide a safer entry point for running Elixir from other platforms
  * [Enumerable] Add `Enum.product_by/2` and `Enum.sum_by/2`
  * [Exception] Add `MissingApplicationsError` exception to denote missing applications
  * [Kernel] Update source code parsing to match [UTS #55](https://www.unicode.org/reports/tr55/) latest recommendations. In particular, mixed script is allowed in identifiers as long as they are separate by underscores (`_`), such as `http_сервер`. Previously allowed highly restrictive identifiers, which mixed Latin and other scripts, such as the japanese word for t-shirt, `Tシャツ`, now require the underscore as well
  * [Kernel] Warn on bidirectional confusability in identifiers
  * [Macro] Improve `dbg` handling of `if/2`, `unless/2`, and code blocks
  * [Process] Handle arbitrarily high integer values in `Process.sleep/1`
  * [String] Inspect special whitespace and zero-width characters using their Unicode representation

#### ExUnit

  * [ExUnit] Support parameterized tests on `ExUnit.Case`

#### IEx

  * [IEx] Add `:dot_iex` support to `IEx.configure/1`

### 2. Bug fixes

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [List] `List.zip/1` is deprecated in favor of `Enum.zip/1`

### 4. Hard deprecations

#### EEx

  * [EEx] `<%#` is deprecated in favor of `<%!--` or `<% #`
  * [EEx] `c:EEx.handle_text/2` is deprecated in favor of `c:EEx.handle_text/3`

#### Elixir

  * [Enumerable] Deprecate returning a two-arity function in `Enumerable.slice/1`
  * [Module] Deprecate `Module.eval_quoted/3` in favor of `Code.eval_quoted/3`
  * [Range] Deprecate inferring negative ranges on `Range.new/2`

#### Mix

  * [mix cmd] Deprecate `mix cmd --app APP` in favor of `mix do --app APP`

## v1.17

The CHANGELOG for v1.17 releases can be found [in the v1.17 branch](https://github.com/elixir-lang/elixir/blob/v1.17/CHANGELOG.md).
