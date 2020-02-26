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

`Enum.sort/1` in Elixir by default sorts from lowest to highest:

```elixir
iex> Enum.sort(["banana", "apple", "pineapple"])
["apple", "banana", "pineapple"]
```

If you want to sort from highest to lowest, you need to call `Enum.sort/2` with a custom sorting function, such as `Enum.sort(collection, &>=/2)`, which is not immediately obvious to someone reading the code:

```elixir
iex> Enum.sort(["banana", "apple", "pineapple"], &>=/2)
["pineapple", "banana", "apple"]
```

Furthermore, comparison operators, such as `<=` and `>=`, perform structural sorting, instead of a semantic one. For example, using `>=` to sort dates descendingly won't yield the correct result:

```elixir
iex> Enum.sort([~D[2019-12-31], ~D[2020-01-01]])
[~D[2020-01-01], ~D[2019-12-31]]
```

To perform proper semantic comparison for dates, one would also need to pass a custom sorting function:

```elixir
iex> Enum.sort([~D[2019-12-31], ~D[2020-01-01]], &(Date.compare(&1, &2) != :lt))
[~D[2019-12-31], ~D[2020-01-01]]
```

Elixir v1.10 streamlines the sorting functions by introducing both `:asc` and `:desc` shortcuts:

```elixir
iex> Enum.sort(["banana", "apple", "pineapple"], :asc)
["apple", "banana", "pineapple"]
iex> Enum.sort(["banana", "apple", "pineapple"], :desc)
["pineapple", "banana", "apple"]
```

As well as adding the possibility to pass a module to perform semantic comparisons. For example, to sort dates, one now only needs to pass the `Date` module or even `{:desc, Date}` for descending semantical sort:

```elixir
iex> Enum.sort([~D[2019-12-31], ~D[2020-01-01]], Date)
[~D[2019-12-31], ~D[2020-01-01]]
iex> Enum.sort([~D[2019-12-31], ~D[2020-01-01]], {:desc, Date})
[~D[2020-01-01], ~D[2019-12-31]]
```

These API improvements make the code more concise and readable and they have also been added to `Enum.sort_by`, `Enum.min_by`, `Enum.max_by`, and friends.

## Tracking of compile-time configuration

In Elixir, we organize our code in applications. Libraries, your dependencies, and your own project are all separate applications. All applications in Elixir also come with an application environment.

The application environment is a key-value store that allows us to configure said application. While reading the application environment at runtime is the preferred approach, in some rare occasions you may want to use the application environment to configure the compilation of a certain project. This is often done by calling `Application.get_env/3` outside of a function:

```elixir
defmodule MyApp.DBClient do
  @db_host Application.get_env(:my_app, :db_host, "db.local")
  def start_link() do
    SomeLib.DBClient.start_link(host: @db_host)
  end
end
```

This approach has one big limitation: if you change the value of the application environment after the code is compiled, the value used at runtime is not going to change! For example, if you are using `mix release` and your `config/releases.exs` has:

    config :my_app, :db_host, "db.production"

Because `config/releases.exs` is read after the code is compiled, the new value will have no effect as the code was compiled to connect to "db.local".

Of course, the obvious solution to this mismatch is to not read the application environment at compilation time in the first place, and instead move the code to inside a function:

```elixir
defmodule MyApp.DBClient do
  def start_link() do
    SomeLib.DBClient.start_link(host: db_host())
  end
  defp db_host() do
    Application.get_env(:my_app, :db_host, "db.local")
  end
end
```

While this is the preferred approach, there are still two scenarios we need to address:

  1. Not everyone may be aware of this pitfall, so they will mistakenly read the application environemnt at compile-time, until they are bitten by this behaviour

  2. In rare occasions, you trully need to read the application environment at compile-time, and you want to be warned when you try to configure at runtime something that is valid only at compilation time

Elixir v1.10 aims to solve these two scenarios by introducing a `Application.compile_env/3` function. For example, to read the value at compile time, you can now do:

```elixir
@db_host Application.compile_env(:my_app, :db_host, "db.local")
```

By using `compile_env/3`, Elixir will store the values used during compilation and compare them with the runtime values whenever your system starts, raising an error in case they differ. This helps developers ensure they are running their production systems with the configuration they intend to.

In future versions, we will deprecate the use `Application.get_env` at compile-time with a clear message pointing users to configuration best practices, effectively addressing the scenario where users read from the application environment at compile time unaware of its pitfalls.

## Compiler tracing

This release brings enhancements to the Elixir compiler and adds new capabilities for developers to listen to compilation events.

In previous Elixir versions, Elixir would compile a database of cross references between modules (such as function calls, references, structs, etc) for each project in order to perform all kinds of checks, such as deprecations and undefined functions.

Although this database was not public, developers would still use it to run their own checks against their projects. With time, developers would request more data to be included in the database, which was problematic as Elixir itself did not have a use for the additional data, and the database was not meant to be used externally in the first place.

In Elixir v1.10, we have addressed these problems by introducing compiler tracing. The compiler tracing allows developers to listen to events as they are emitted by the compiler, so they can store all of the information they need - and only the information they need.

Elixir itself is using the new compiler tracing to provide new functionality. One advantage of this approach is that developers can now disable undefined function warnings directly on the callsite. For example, imagine you have an optional dependency which may not be available in some cases. You can tell the compiler to skip warning on calls to optional modules with:

    @compile {:no_warn_undefined, OptionalDependency}
    defdelegate my_function_call(arg), to: OptionalDependency

Previously, this information had to be added to the overall project configuration, which was far away from where the optional call effectively happened.

## Other enhancements

Elixir's calendar data types got many improvements, such as sigil support for third-party calendars, as well as the additions of `DateTime.now!/2`, `DateTime.shift_zone!/3`, and `NaiveDateTime.local_now/0`.

There are many improvements related to Elixir's AST in this release too. First of all, `Code.string_to_quoted/2` has two new options, `:token_metadata` and `:literal_encoder`, that give more control over Elixir's parser. This information was already available to the Elixir code formatter and has now been made public. We have also extensively documented all of Elixir's AST metadata. These changes alongside compiler tracing means static analyzers and IDE integrations have a better foundation to analyze the source code.

ExUnit, our test framework, ships two small but important improvements: `ExUnit.CaptureIO` can now be used by tests that run concurrently and we have added "pattern-matching diffing". To understand the last feature, take this code:

```elixir
assert %{"status" => 200, "body" => %{"key" => "foo"}} = json_payload
```

Now imagine that `json_payload` is a large JSON blob and the `"key"` inside the `"body"` did not have value of `"foo"`. In previous Elixir versions, if the assertion failed, Elixir would print the right side and let you up to your own devices to figure out what went wrong. In Elixir v1.10, we diff the data structure against the pattern so you can see exactly which parts of the data matched the pattern and which ones did not. Note ExUnit already performed diffing when comparing data types, this new version adds diffing when matching data against a pattern.

## v1.10.2 (2020-02-26)

### 1. Bug fixes

#### Elixir

  * [Macro] Fix a bug where `Macro.to_string/1` would emit invalid code for sigils
  * [Task] Do not crash `async_stream` monitor if it receives spurious DOWN messages

#### Logger

  * [Logger] Fix a bug where the Logger formatter would fail when handling unknown metadata values

#### Mix

  * [mix compile] Do not write files to disk if `--warnings-as-errors` was given and warnings were emitted

## v1.10.1 (2020-02-10)

### 1. Bug fixes

#### Elixir

  * [Code] Do not emit invalid code when formatting `nil`, `false`, and `true` keys in maps
  * [Kernel] Ensure `with` clauses properly unpack "implicit guards" (such as matching on the struct name)
  * [Kernel] Do not warn if commas are used by themselves in `~w`/`~W` sigils
  * [Kernel] Do not validate the `:line` option in quote (the validation has been moved to v1.11 to give users more time to update their code)
  * [Module] Ensure the code verifier handles the `:erlang.size/1` guard properly

#### Logger

  * [Logger] Properly handle the `report_cb/2` option from Erlang
  * [Logger] Fix truncation for multi-byte characters
  * [Logger] Do not rebroadcast messages from remote nodes as this is now taken care by Erlang's logger

#### ExUnit

  * [ExUnit] Ensure `assert_receive` produces valid exception messages in case of errors

#### Mix

  * [mix release] Make sure the install command (Window specific) works on paths with spaces in the name
  * [mix release] Allow using `remote` and `rpc` commands with `Application.compile_env/3`

## v1.10.0 (2020-01-27)

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
  * [Kernel] Allow file, line and context to be dynamically set on `quote`
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
  * [mix deps.compile] Add `--skip-umbrella-children` flag. The new flag does not compile umbrella apps. This is useful for building caches in CD/CI pipelines
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
  * [Kernel] Do not expand expressions inside interpolation twice
  * [Keyword] Ensure keyword replace and update preserve order
  * [Module] Raise instead of silently failing when performing a write module operation during after-compile
  * [Module] Fix `@macrocallback` definitions with a `when` clause
  * [Path] Fix `Path.absname/1` to correctly handle UNC paths on Windows
  * [Stream] Close with correct accumulator in `Stream.resource/3` when called for a single-element list
  * [Stream] Allow `Stream.cycle/1` to be double nested inside `Stream.cycle/1`
  * [URI] Preserve slashes in URIs without authority
  * [URI] Require a nil or an absolute path on URIs with host or authority

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

  * [Mix.Config] `Mix.Config.persist/1` has been deprecated. Instead of `Mix.Config.persist(config)` use `Application.put_all_env(config, persistent: true)` (`Application.put_all_env/2` was added in v1.9)
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
