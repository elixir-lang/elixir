# Changelog for Elixir v1.15

This release requires Erlang/OTP 24 and later.

Elixir v1.15 is a smaller release with focused improvements
on compilation and boot times. This release also completes
our integration process with Erlang/OTP logger, bringing new
features such as log rotation and compaction out of the box.

Finally, you will also find additional convenience functions in
`Map`, `Keyword`, all Calendar modules, and others.

## Compile and boot-time improvements

The last several releases brought improvements to compilation
time and this version is no different. In particular, this version
now caches and prunes load paths before compilation, ensuring your
project (and dependencies!) compile faster and in an environment
closer to production.

In a nutshell the Erlang VM loads modules from code paths. Each
application that ships with Erlang and Elixir plus each dependency
end-up being an entry in your code path. The larger the code path,
the more work Erlang has to do in order to find a module.

In previous versions, Mix would only add entries to the load paths.
Therefore, if you compiled 20 dependencies and you went to compile
the 21st, the code path would have 21 entries (plus all Erlang and
Elixir apps). This allowed modules from unrelated dependencies to
be seen and made compilation slower the more dependencies you had.

In this release, we will now prune the code paths to only the ones
listed as dependencies. Previously if you attempted to use an
Erlang/OTP or Elixir module without adding its dependency, we would warn.
Now the module won't be found altogether (instead of warning as in
previous versions), which is also the behaviour you see if you ran
your application as a `mix release`. If you are using an application
that does not correctly lists its dependencies, they will have to
be updated accordingly (as previously warned). You can temporarily
disable this new behaviour by setting `prune_code_paths: false` in
your `mix.exs`.

Furthermore, Erlang/OTP 26 allows us to start applications concurrently
and cache the code path lookups, decreasing the cost of booting applications.
The combination of Elixir v1.15 and Erlang/OTP 26 should reduce the boot
time of applications, such as when starting `iex -S mix` or running a single
test with `mix test`, from 5% to 15%.

## Compiler warnings and errors

TODO.

## Integration with Erlang/OTP logger

This provides additional features such as global logger metadata and
file logging (with rotation and compaction) out-of-the-box! See
the `Logger` documentation for more information.

TODO: Mention :console vs Logger.Backends.Console

## v1.15.0-dev

### 1. Enhancements

#### EEx

  * [EEx] Include source code snippets in syntax errors

#### Elixir

  * [Calendar] Add support for epoch time (%s) to `Calendar.strftime/2`
  * [Code] `Code.format_string!/2` now converts `'charlists'` into `~c"charlists"` by default
  * [Code] Add `:on_undefined_variable` to the compiler options to preserve the warning behaviour which was deprecated back in Elixir v1.4
  * [Code] Add `Code.loaded?/1` and `Code.ensure_all_loaded(!)/1`
  * [Code] Add `Code.prepend_paths/1`, `Code.append_paths/1`, and `Code.delete_paths/1`
  * [Code] Support nested expressions in `Code.cursor_context/1`
  * [Date] Add `Date.before?/2` and `Date.after?/2`
  * [DateTime] Add `DateTime.before?/2` and `DateTime.after?/2`
  * [Inspect] `Inspect` now renders `'charlists'` as `~c"charlists"` by default
  * [Kernel] Break down `case` and `cond` inside `dbg/2`
  * [Kernel] Add `t:nonempty_binary/0` and `t:nonempty_bitstring/0`
  * [Kernel] Treat `@behaviour`s as runtime dependencies
  * [Kernel] Do not add runtime dependencies for alias references in patterns and guards
  * [Kernel] Warn for nested calls without parens inside keywords
  * [Kernel] Support for multi-letter uppercase sigils
  * [Kernel] Introduce mechanism to collect several errors in a module. Previously, as soon as there was a compilation error, compilation would fail. Now the compiler became a bit smarter and will report multiple errors whenever possible as multiple `error: ...` messages, similar to `warning: ...`
  * [Kernel.CLI] Support `--sname undefined`/`--name undefined` so a name is automatically generated
  * [Keyword] Add `Keyword.split_with/2`
  * [Macro] Improve error message when piping into an expression ending in bracket-based access
  * [Macro.Env] Add `Macro.Env.lookup_alias_as/2`
  * [Map] Add `Map.split_with/2`
  * [Map] Add `Map.intersect/2` and `Map.intersect/3`
  * [MapSet] Add `MapSet.split_with/2`
  * [MapSet] Optimize most functions
  * [NaiveDateTime] Add `NaiveDateTime.beginning_of_day/1` and `NaiveDateTime.end_of_day/1`
  * [NaiveDateTime] Add `NaiveDateTime.before?/2` and `NaiveDateTime.after?/2`
  * [OptionParser] Support `:return_separator` option
  * [Process] Add `Process.alias/0,1` and `Process.unalias/1`
  * [Range] Add `Range.split/2`
  * [String] Update Unicode to version 15.0.0
  * [String] Add `:fast_ascii` mode to `String.valid?/2`
  * [Supervisor] Add support for automatic shutdown in `Supervisor`
  * [System] Support `:lines` in `System.cmd/3` to capture output line by line
  * [Task] Remove head of line blocking on `Task.yield_many/2`
  * [Task] Enable selective receive optimizations in Erlang/OTP 26+
  * [Task.Supervisor] Do not copy args on temporary `Task.Supervisor.start_child/2`
  * [Time] Add `Time.before?/2` and `Time.after?/2`
  * [URI] Add `URI.append_path/2`

#### ExUnit

  * [ExUnit] Add more color configuration to ExUnit CLI formatter
  * [ExUnit.Callbacks] Accept `{module, function}` tuples in ExUnit `setup` callbacks
  * [ExUnit.Doctest] Add `ExUnit.DocTest.doctest_file/2`
  * [ExUnit.Formatter] When comparing to anonymous functions, defined at the same place but capturing a different environment, we will now also diff the environments

#### IEx

  * [IEx] Make pry opt-in on dbg with `--dbg pry`
  * [IEX] Support `IEX_HOME`
  * [IEx.Helpers] Add `runtime_info(:allocators)`
  * [IEx.Info] Implement protocol for `Range`, `DateTime` and `Regex`

#### Logger

  * [Logger] Add `Logger.add_handlers/1` and `Logger.default_formatter/1`
  * [Logger] Introduce `default_formatter` and `default_handler` configuration for Logger which configures Erlang/OTP logger
  * [Logger.Formatter] Implement the Erlang Logger formatter API
  * [Logger.Formatter] Add support for ports in Logger metadata

#### Mix

  * [Mix.Project] Support `def cli` to unify all CLI defaults in a single place
  * [Mix.Project] Add `Mix.Project.deps_tree/1`
  * [mix eval] Allow passing additional arguments
  * [mix compile] Set `--all-warnings` by default
  * [mix compile] Reduce the amount of filesystem lookups for path dependencies by storing timestamps in manifests
  * [mix compile.app] Write `optional_applications` to `.app` file
  * [mix compile.elixir] Add `--purge-consolidation-path-if-stale` which will purge the given consolidation path if compilation is required
  * [mix deps.get] Automatically install Hex and Rebar on `mix deps.get`/`mix deps.update`
  * [mix deps.get] Support `--check-locked` which raises if changes to the lockfile are required
  * [mix format] Allow multiple formatters per file extension and sigil
  * [mix format] Show diffs whenever `--check-formatted` fails
  * [mix profile.fprof] Support `--trace-to-file` to improve performance when working with large outputs
  * [mix release] Allow passing additional arguments to the `eval` command
  * [mix xref graph] Support `--output` flag

### 2. Bug fixes

#### Elixir

  * [Code.Formatter] Fix a scenario where a keyword followed by parenthesis could go above the maximum line length
  * [Code.Formatter] Remove unnecessary parens in nullary type funs
  * [Exception] Fix operator precedence when printing guards in `Exception.blame/3`
  * [File] Do not raise if there are file system race conditions in `File.cp/2`
  * [Kernel] Expand macros on the left side of -> in `try/rescue`
  * [Kernel] Raise on misplaced `...` inside typespecs
  * [Kernel.ParallelCompiler] Make sure compiler doesn't crash when there are stray messages in the inbox
  * [Kernel.ParallelCompiler] Track compile and runtime warnings separately
  * [URI] Make sure `URI.merge/2` works accordingly with relative paths

#### ExUnit

  * [ExUnit] Fix crash when `@tag capture_log: true` was set to true and the Logger application was shut down in the middle of the test
  * [ExUnit] Do not merge context as tags inside the runner to reduce memory usage when emitting events to formatters
  * [ExUnit] Do not expand or collect vars from quote in ExUnit assertions

#### IEx

  * [IEx] Do not spawn a process to read IO. This fixes a bug where multiline paste stopped working
    whenever the input reader was killed
  * [IEx] Do not perform completion for prompts triggered during code evaluation

#### Mix

  * [mix compile] Include `cwd` in compiler cache key
  * [mix release] Fix Windows service when invoking `erlsrv.exe` in path with spaces

### 3. Soft deprecations (no warnings emitted)

#### Mix

  * [Mix.Project] `:preferred_cli_env` is deprecated in favor of `:preferred_envs` in `def cli`
  * [Mix.Project] `:preferred_cli_target` is deprecated in favor of `:preferred_targets` in `def cli`
  * [mix local] The environment variable `HEX_MIRROR` is deprecated in favor of `HEX_BUILDS_URL`

### 4. Hard deprecations

#### Elixir

  * [Calendar] `Calendar.ISO.day_of_week/3` is deprecated in favor of `Calendar.ISO.day_of_week/4`
  * [Exception] `Exception.exception?/1` is deprecated in favor of `Kernel.is_exception/1`
  * [Kernel] Deprecate `...` as a valid function call identifier
  * [Regex] `Regex.regex?/1` is deprecated in favor of `Kernel.is_struct/2`

#### Logger

  * [Logger] `Logger.warn/2` is deprecated in favor of `Logger.warning/2`

## v1.14

The CHANGELOG for v1.14 releases can be found [in the v1.14 branch](https://github.com/elixir-lang/elixir/blob/v1.14/CHANGELOG.md).
