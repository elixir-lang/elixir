# Changelog for Elixir v1.10

## v1.10.0-dev

### 1. Enhancements

#### Elixir

  * [Code] Add `:token_metadata` and `:literal_encoder` support to `Code.string_to_quoted/2`
  * [DateTime] Add `DateTime.now!/2` and `DateTime.shift_zone!/3`
  * [Module] Add `Module.has_attribute?/2`
  * [Module] Add `@compile {:no_warn_undefined, mfa_or_module}` to turn off undefined function warnings
  * [String] Update to Unicode 12.1

#### Mix

  * [mix deps.unlock] Add `--check-unused` flag

### 2. Bug fixes

#### IEx

  * [IEx] Exit IEx session if the group leader exits

#### Mix

  * [Mix.Project] Make sure `MIX_BUILD_PATH` specifies only the `build_path` prefix and that env+target are still concatenated

### 3. Soft-deprecations (no warnings emitted)

#### Elixir

  * [Code] `compiler_options/0` is deprecated in favor of `compiler_option/1`

#### Mix

  * [mix xref] `calls/0` is deprecated in favor of upcoming compiler tracer
  * [mix xref] The `xref.exclude` option has been moved to `elixirc_options.no_warn_undefined` as the `xref` pass has been moved into the compiler

### 4. Hard-deprecations

#### Mix

  * [mix compile.xref] This check has been moved into the compiler and has no effect now
  * [mix xref deprecations] This check has been moved into the compiler and has no effect now
  * [mix xref unreachable] This check has been moved into the compiler and has no effect now

#### Elixir

  * [Code] `Code.load_file/2` has been deprecated in favor of `Code.require_file/2` or `Code.compile_file/2`
  * [Code] `Code.loaded_files/0` and `Code.unload_file/1`  have been deprecated in favor of `Code.required_files/0` and `Code.unrequire_file/1` respectively
  * [String] `String.normalize/2` has been deprecated in favor of `:unicode.characters_to_nfc_binary/1` or `:unicode.characters_to_nfd_binary/1` which ship as part of Erlang/OTP 20+
  * [Supervisor] The `Supervisor.Spec` module and its functions have been deprecated in favor of the new Supervisor child specification
  * [Supervisor] The `:simple_one_for_one` strategy in `Supervisor` has been deprecated in favor of `DynamicSupervisor`

#### Logger

  * [Logger] `:compile_time_purge_level` application environment configuration has been deprecated in favor of the more general `:compile_time_purge_matching` config

## v1.9

The CHANGELOG for v1.9 releases can be found [in the v1.9 branch](https://github.com/elixir-lang/elixir/blob/v1.9/CHANGELOG.md).
