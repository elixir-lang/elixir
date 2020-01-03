# Changelog for Elixir v1.10

## Support for Erlang/OTP 21+

Elixir v1.10 requires Erlang/OTP 21+, allowing Elixir to integrate with Erlang/OTP's new logger. Currently, this means that the logger level, logger metadata, as well as all log messages are now shared between Erlang and Elixir APIs.

We will continue improving the relationship between the logging systems in future releases. In particular, we plan to expose all log levels and runtime filtering functionalities available in Erlang directly into Elixir in the next Elixir version.

This release also adds two new guards, `is_struct/1` and `is_map_key/2`, thanks to the strict requirement on Erlang/OTP 21+.

## Releases improvements

Elixir v1.9 introduced releases as a mechanism to package self-contained applications. Elixir v1.10 further improves releases with bug fixes and new enhancements based on feedback we got from the community. The highlights are:

  * Allow the dual boot system of releases to be disabled on environments that are boot-time sensitive, such as embedded devices

  * Track and raise if compile-time configuration is set or changes at runtime (more in the next section)

  * Support for easily adding extra files to releases via overlays

  * Allow `RELEASE_DISTRIBUTION` to be set to `none` in order to fully disable it

  * Add a built-in `:tar` step that automatically packages releases

See the full CHANGELOG for more improvements.

## Improvements to sort-based APIs in Enum

`Enum.sort/1` in Elixir always sorts from lowest to highest. If you want to sort from highest to lowest, you need to call `Enum.sort/2` with a custom sorting function, such as `Enum.sort(collection, &>=/2)`, which is not immediately obvious to someone reading the code.

To make matters worse, comparison operators, such as `<=` and `>=`, perform structural sorting, instead of a semantic one. For example, using `>=` to sort dates descendingly won't yield the correct result. Therefore, to sort dates from more recent to oldest, one has to write `Enum.sort(dates, &(Date.compare(&1, &2) != :lt))`.

Elixir v1.10 streamlines the sorting functions by introducing both `:asc` and `:desc` shortcuts:

    Enum.sort(collection, :asc) # the default
    Enum.sort(collection, :desc) # in reverse

Furthermore, if you want to perform semantic comparison, you can pass a module that provides the relevant comparison function. For example, to sort dates:

    Enum.sort(birth_dates, Date)
    Enum.sort(birth_dates, {:asc, Date})
    Enum.sort(birth_dates, {:desc, Date})

This new API has also been added to `Enum.sort_by`, `Enum.min_by`, `Enum.max_by`, and friends.

### Tracking of compile-time configuration

All applications in Elixir come with an application environment. This environment is a key-value store that allows us to configure said application. While reading the application environment at runtime is the preferred approach, in some rare occasions you may want to use the application environment to configure the compilation of a certain project. This is often done by calling `Application.get_env/3` outside of a function:

    defmodule MyApp.DBClient do
      @db_host Application.get_env(:my_app, :db_host, "db.local")

      def start_link() do
        SomeLib.DBClient.start_link(host: @db_host)
      end
    end

This approach has one big limitation: if you change the value of the application environment after the code is compiled, the value used at runtime is not going to change! For example, if you are using `mix release` and your `config/releases.exs` has:

    config :my_app, :db_host, "db.production"

The new value will have no effect as the code was compiled to connect to "db.local", which is mostly likely unavailable in the production environment.

For those reasons, reading the application environment at runtime should be the first choice. However, if you really have to read the application environment during compilation, Elixir v1.10 introduces a `Application.compile_env/3` function:

    @db_host Application.compile_env(:my_app, :db_host, "db.local")

By using `compile_env/3`, Elixir will store the values used during compilation and compare the compilation values with the runtime values whenever your system starts, raising an error in case they differ. This helps developers ensure they are running their production systems with the configuration they intend to.

### Compiler tracing

This release brings enhancements to the Elixir compiler and adds new capabilities for developers to listen to compilation events.

In previous Elixir releases, Elixir would compile a database of cross references between modules (such as function calls, references, structs, etc) for each project. Although developers could traverse this database, they often requested more events or more information to be made available.

In Elixir v1.10, we have replaced this database by compiler tracing. This means that developers can now directly listen to events emitted by the compiler to store and collect all the information they need (and only the information they need).

Elixir itself is already using the new compiler tracing to provide new functionality. In particular, the compiler now checks for undefined function warnings more consistently. In previous versions, we would emit undefined function warnings only for files in `lib`, skipping test files and scripts.

Furthermore, in Elixir v1.10 developers can now disable undefined function warnings directly on the callsite. For example, imagine you have an optional dependency which may not be available in some cases. You can tell the compiler to skip warning on calls to optional modules with:

    @compile {:no_warn_undefined, OptionalDependency}
    defdelegate my_function_call(arg), to: OptionalDependency

Finally, as consequence of these improvements, some functionality related to `xref` (our previous database), has been deprecated in favor of the new compiler tracing.

### Other enhancements

The calendar data types got many improvements, such as sigil support for third-party calendars, as well as the additions of `DateTime.now!/2`, `DateTime.shift_zone!/3`, and `NaiveDateTime.local_now/0`.

There are many improvements related to the Elixir AST in this release too. First of all, `Code.string_to_quoted/2` has two new options, `:token_metadata` and `:literal_encoder`, that give more control over Elixir's parser. This information has already been available to the Elixir formatter for a couple versions and has now been made public. Furthermore, all public metadata entries in the AST nodes have been extensively documented. These changes alongside the compiler improvements from previous section means tools like Credo and Boundary now have a better foundation to analyze the source code.

Finally, ExUnit comes with two small but important improvements: `ExUnit.CaptureIO` can now be used in tests that run asynchronously and we have added "data-structure diffing" when performing assertions with pattern matching. So now, whenever an assertion such `assert %{field: value} = expression()` fails, ExUnit will show both left-hand and right-hand sides, highlighting the parts that did not match in red.

## v1.10.0

### 1. Enhancements

#### Elixir

  * [Application] Add `Application.compile_env/3` and `Application.compile_env!/2` for reading values at compilation time and tracking if they accidentally change during runtime
  * [Calendar] Allow custom calendar representations in calendar sigils
  * [Calendar] Add `c:Calendar.parse_time/1`, `c:Calendar.parse_date/1`, `c:Calendar.parse_naive_datetime/1` and `c:Calendar.parse_utc_datetime/1` callbacks to calendar behaviour
  * [CLI] Add support for `NO_COLOR` environment variable
  * [Code] Add `:token_metadata` and `:literal_encoder` support to `Code.string_to_quoted/2`
  * [Code] Add compiler tracing to lift events done by the compiler
  * [Code] Return `{:error, :unavailable}` in `Code.ensure_compiled/1` if module is in a deadlock
  * [DateTime] Add `DateTime.now!/2` and `DateTime.shift_zone!/3`
  * [Enum] Speed up getting one random element from enumerables
  * [Enum] Add `Enum.frequencies/1`, `Enum.frequencies_by/2`, and `Enum.map_intersperse/2`
  * [Enum] Allow a sorting function on `Enum.min/max/min_by/max_by`
  * [Enum] Add `asc/desc` and `compare/1` support to `Enum.sort/2`
  * [Exception] Add version alongside app names in stacktraces
  * [Function] Add `Function.identity/1`
  * [Kernel] Add `Kernel.is_struct/1` and `Kernel.is_map_key/2`
  * [Kernel] Warn when function head comes immediately after the implementation instead of before the implementation
  * [Kernel] Warn if duplicate key is found in struct declaration
  * [Kernel] Print all undefined functions as warnings and then raise. This allows users to see all undefined calls at once, when it would otherwise require them to compile the code multiple times
  * [Keyword] Add `Keyword.pop!/2` and `Keyword.pop_values/2`
  * [Map] Add `Map.pop!/2`
  * [MapSet] Optimize multiple operations
  * [Module] Add `Module.has_attribute?/2`
  * [Module] Add `@compile {:no_warn_undefined, mfa_or_module}` to turn off undefined function warnings
  * [NaiveDateTime] Add `NaiveDateTime.local_now/0`
  * [Record] Warn if duplicate key is found in record declaration
  * [String] Update to Unicode 12.1
  * [StringIO] Add `:encoding` option to StringIO and optimize `get_chars` operation

#### ExUnit

  * [ExUnit.Assertions] Support diffs in pattern matching and in `assert_receive`
  * [ExUnit.CaptureIO] Supports capturing named devices in asynchronous tests

#### IEx

  * [IEx] Warn on circular file imports when loading default `.iex.exs`
  * [IEx] Allow customization of the continuation prompt on IEx

#### Logger

  * [Logger] Allow `start_options` to be configured on Logger's GenEvent
  * [Logger] Integrate Elixir's Logger with Erlang/OTP 21+'s logger. This means setting up the logger level in Elixir will automatically change the logger level for Erlang and vice-versa

#### Mix

  * [mix compile] Add `--profile time` flag to profile compilation steps
  * [mix deps.compile] Add `--skip-umbrella-apps` flag. The new flag does not compile umbrella apps. This is useful for building caches in CD/CI pipelines
  * [mix deps.unlock] Add `--check-unused` flag. The new flag raises if there are any unused dependencies in the lock file
  * [mix release] Allow `RELEASE_DISTRIBUTION` to be set to `none`
  * [mix release] Support overlays in `rel/overlays`
  * [mix release] Allow configuration reboot to be disabled in releases
  * [mix test] Add support for simple round-robin test partitioning across multiple machines
  * [Mix.Project] Add `MIX_DEPS_PATH` environment variable for setting `:deps_path`
  * [Mix.Project] Add `Mix.Project.deps_scms/1` that returns deps with their SCMs
  * [Mix.Task] Add `Mix.Task.Compiler.after_compiler/2` callback, to simplify compilers that may need to run something at multiple steps

### 2. Bug fixes

#### EEx

  * [EEx] Ensure multiline do/end with no spaces compile under trim mode

#### Elixir

  * [Enum] Allow positive range slices on infinite streams given to `Enum.slice/2`
  * [Kernel] Raise error on functions/guards without implementation
  * [Keyword] Ensure keyword replace and update preserve order
  * [Module] Raise instead of silently failing when performing a write module operation during after-compile
  * [Module] Fix `@macrocallback` definitions with a `when` clause
  * [Stream] Close with correct accumulator in `Stream.resource/3` when called for a single-element list
  * [Stream] Allow `Stream.cycle/1` to be double nested inside `Stream.cycle/1`
  * [URI] Preserve slashes in URIs without authority

#### IEx

  * [IEx] Exit IEx session if the group leader exits
  * [IEx] Allow `pry` to be used in non-tty terminals

#### Mix

  * [mix compile] Do not filter out warning for external files from diagnostics
  * [Mix.Project] Ensure user given `:manager` to dependencies has higher precedence than the SCM one
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
