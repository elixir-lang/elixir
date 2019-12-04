# Changelog for Elixir v1.10

## v1.10.0-dev

Elixir v1.10 requires Erlang/OTP 21+.

### 1. Enhancements

#### Elixir

  * [Calendar] Allow custom calendar representations in calendar sigils
  * [Calendar] Add `c:Calendar.parse_time/1`, `c:Calendar.parse_date/1`, `c:Calendar.parse_naive_datetime/1` and `c:Calendar.parse_utc_datetime/1` callbacks to calendar behaviour
  * [Code] Add `:token_metadata` and `:literal_encoder` support to `Code.string_to_quoted/2`
  * [Code] Add compiler tracing to lift events done by the compiler
  * [DateTime] Add `DateTime.now!/2` and `DateTime.shift_zone!/3`
  * [Enum] Speed up getting one random element from enumerables
  * [Enum] Add `Enum.frequencies/1`, `Enum.frequencies_by/2`, and `Enum.map_intersperse/2`
  * [Enum] Allow a sorting function on `Enum.min/max/min_by/max_by`
  * [Enum] Add `asc/desc` and `compare/1` support to `Enum.sort/2`
  * [Exception] Add version alongside app names in stacktraces
  * [Function] Add `Function.identity/1`
  * [Kernel] Add `Kernel.is_struct/1`
  * [Kernel] Warn when function head comes immediately after the implementation instead of before the implementation
  * [Kernel] Warn if duplicate key is found in struct declaration
  * [Keyword] Add `Keyword.pop!/2` and `Keyword.pop_values/2`
  * [Map] Add `Map.pop!/2`
  * [MapSet] Optimize multiple operations
  * [Module] Add `Module.has_attribute?/2`
  * [Module] Add `@compile {:no_warn_undefined, mfa_or_module}` to turn off undefined function warnings
  * [Record] Warn if duplicate key is found in record declaration
  * [String] Update to Unicode 12.1
  * [StringIO] Add `:encoding` option to StringIO and optimize `get_chars` operation

#### ExUnit

  * [ExUnit.Assertions] Support diffs in pattern matching and in `assert_receive`
  * [ExUnit.CaptureIO] Supports capturing named devices in asynchronous tests

#### Logger

  * [Logger] Allow `start_options` to be configured on Logger's GenEvent
  * [Logger] Integrate Elixir's Logger with Erlang/OTP 21+'s logger. This means setting up the logger level in Elixir will automatically change the logger level for Erlang and vice-versa

#### Mix

  * [mix deps.compile] Add `--skip-umbrella-apps` flag. The new flag does not compile umbrella apps. This is useful for building caches in CD/CI pipelines
  * [mix deps.unlock] Add `--check-unused` flag. The new flag raises if there are any unused dependencies in the lock file
  * [mix release] Allow `{:from_app, app_name}` as a version for releases
  * [mix test] Add support for simple round-robin test partitioning across multiple machines
  * [Mix.Project] Add `MIX_DEPS_PATH` environment variable for setting `:deps_path`
  * [Mix.Project] Add `Mix.Project.deps_scms/1` that returns deps with their SCMs
  * [Mix.Task] Add `Mix.Task.Compiler.after_compiler/2` callback, to simplify compilers that may need to run something at multiple steps

#### IEx

  * [IEx] Warn on circular file imports when loading default `.iex.exs`

### 2. Bug fixes

#### Elixir

  * [Kernel] Raise error on functions/guards without implementation
  * [Keyword] Ensure keyword replace and update preserve order
  * [Module] Raise instead of silently failing when performing a write module operation during after-compile

#### IEx

  * [IEx] Exit IEx session if the group leader exits
  * [IEx] Allow `pry` to be used in non-tty terminals

#### Mix

  * [Mix.Project] Make sure `MIX_BUILD_PATH` specifies only the `build_path` prefix and that env+target are still concatenated
  * [Mix.Project] Recompile umbrella children when config files change and `mix compile` is called from the umbrella root
  * [Mix.Task] Always recompile before running tasks from dependencies
  * [Mix.Task] Ensure project's Logger config is used when running Mix tasks

### 3. Soft-deprecations (no warnings emitted)

#### Elixir

  * [Code] `compiler_options/0` is deprecated in favor of `compiler_option/1`

#### Mix

  * [mix xref] `calls/0` is deprecated in favor of compiler tracer
  * [mix xref] The `xref.exclude` option has been moved to `elixirc_options.no_warn_undefined` as the `xref` pass has been moved into the compiler

### 4. Hard-deprecations

#### Elixir

  * [Code] `Code.load_file/2` has been deprecated in favor of `Code.require_file/2` or `Code.compile_file/2`
  * [Code] `Code.loaded_files/0` and `Code.unload_file/1`  have been deprecated in favor of `Code.required_files/0` and `Code.unrequire_file/1` respectively
  * [Code] `Code.ensure_compiled?/1` is deprecated in favor of `Code.ensure_compiled/1`
  * [String] `String.normalize/2` has been deprecated in favor of `:unicode.characters_to_nfc_binary/1` or `:unicode.characters_to_nfd_binary/1` which ship as part of Erlang/OTP 20+
  * [Supervisor] `Supervisor.Spec.supervise/2` has been deprecated in favor of the new Supervisor child specification
  * [Supervisor] The `:simple_one_for_one` strategy in `Supervisor` has been deprecated in favor of `DynamicSupervisor`

#### Logger

  * [Logger] `:compile_time_purge_level` application environment configuration has been deprecated in favor of the more general `:compile_time_purge_matching` config
  * [Logger] Deprecate logging non-chardata values

#### Mix

  * [mix compile.xref] This check has been moved into the compiler and has no effect now
  * [mix xref deprecations] This check has been moved into the compiler and has no effect now
  * [mix xref unreachable] This check has been moved into the compiler and has no effect now

## v1.9

The CHANGELOG for v1.9 releases can be found [in the v1.9 branch](https://github.com/elixir-lang/elixir/blob/v1.9/CHANGELOG.md).
