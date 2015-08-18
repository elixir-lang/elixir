# Changelog for Elixir v1.1

v1.1 brings enhancements, bug fixes, performance improvements and more
into Elixir.

Elixir v1.1 supports both Erlang 17 and Erlang 18 and, for this reason,
it does not introduce any feature that is specific to Erlang 18. Such
will be tackled on the follow up Elixir v1.2 release.

On the enhancements side, the most notable changes are the new functions
added to `Enum`, `Dict` and `Task` modules, and a new datatype called `MapSet`.
`MapSet` implements the `Set` API on top of a map and, for Elixir v1.1,
it is useful for holding only dozens of entries. Future Elixir versions,
however, will be able to rely on `MapSet` from dozens of keys up to
millions, with better performance than `HashSet`.

On the deprecation side, this release includes one major deprecation and
some soft deprecations.

The major deprecation relates to the Access protocol. Due to performance
issues, the access syntax `opts[key]` will no longer be powered by the
`Access` protocol, instead, it will use the `Dict` module. Therefore this
release will emit warnings if you attempt to implement the `Access` protocol.
Note the `Access` module and the `opts[key]` syntax are not affected and
they are not deprecated, only the underlying protocol dispatch.

The soft deprecations are minor and they won't emit warnings. It simply
means the documentation has been updated to mention the new best
practices. Warnings will be emitted in the future though (when they are
promoted to deprecations).

On the tooling side, ExUnit got the ability to skip tests and a couple
new configuration options. Mix got improved warnings and error messages,
faster compilation times and the brand new `mix profile.fprof` task.

Note: Erlang 17.1 contains a regression in its wildcard implementation that
causes tools like rebar to fail. If you have a project with rebar dependencies
and is using Erlang 17.1, remember to update to at least Erlang 17.3.

## v1.1.0-dev

### 1 Enhancements

#### Elixir

  * [Application] Add `Application.fetch_env!/2`
  * [CLI] Add support for `--werl` in Windows bash-like shells
  * [Dict] Add `Dict.get_and_update/3` which behaves similar to the now deprecated Access protocol
  * [Dict] Add `Dict.get_lazy/3`, `Dict.pop_lazy/3` and `Dict.put_new_lazy/3`
  * [Enum] Add `Enum.random/1`, `Enum.take_random/2`, `Enum.min_max/1`, `Enum.min_max_by/2`, `Enum.reverse_slice/3`, `Enum.dedup/1` and `Enum.dedup_by/2`
  * [Enum] Inline common map usage in `Enum` functions for performance
  * [File] Add `File.lstat/1` and `File.lstat/1` that works like `File.stat/1` but is able to return symlink information (i.e. it does not traverse symlinks)
  * [Integer] Add `Integer.digits/2` and `Integer.undigits/2`
  * [Inspect] Add the `:safe` option to `inspect/2` and make it safe by default, meaning failures while inspecting won't trigger other failures. Instead, it will be wrapped in an exception which is properly formatted
  * [IO] Support fenced code blocks on `IO.ANSI.Docs`
  * [GenServer] Add `GenServer.whereis/1` that expands `GenServer` dispatches into a proper pid
  * [Kernel] No longer include `:crypto` and `:syntax_tools` as dependencies. The former is only needed if you have encrypted debug info (therefore you can add `:crypto` as a dependency manually) and the latter is no longer used
  * [Kernel] Raise when `var.Alias` syntax is used and it does not expand to an atom at compile time (previously it emitted warnings)
  * [Kernel] Improve generation of argument names for function signatures
  * [Kernel] `::/2` is now a special form
  * [Kernel] Warn when a variable with underscore is used
  * [Kernel] Allow underscores in binary, octal and hex literals
  * [Kernel] Warn when module attributes, variables, strings and numbers are used in code but the expression has no effect
  * [Kernel] Support `\uXXXX` and `\u{X*}` in strings and char lists to map to Unicode codepoints
  * [List] Add `List.keytake/3`
  * [Module] Improve name inference for function signatures in documentation metadata
  * [Process] Add `Process.hibernate/3`
  * [Set] Introduce `MapSet` data type. This new data type uses maps behind the scenes and is useful for storing a dozens of items in Erlang 17. In future versions when maps efficiently support large collections, it is meant to be the main Set abstraction in Elixir
  * [Stream] Add `Stream.dedup/1` and `Stream.dedup_by/2`
  * [String] Support calculation of the jaro distance between strings (usually names) via `String.jaro_distance/2`. This is used by Mix to support "Did you mean?" feature when a task does not exist
  * [String] Add `String.splitter/3` that splits strings as a stream
  * [StringIO] `StringIO.flush/1` was added to flush the output of a StringIO device
  * [Task] Introduce `Task.yield/2` and `Task.shutdown/2` to check if a task is still executing and shutdown otherwise
  * [Tuple] Add `Tuple.append/2`
  * [URI] Default ports were added for "ws" and "wss" schemas

#### EEx

  * [EEx] Add `:trim` option to EEx that automatically trims the left side of `<%` and right side `%>` if only spaces and new lines preceed/follow them

#### ExUnit

  * [ExUnit] Add number of skipped tests to `ExUnit` output
  * [ExUnit] Make timeout configurable for the whole test suite via the `:timeout` configuration
  * [ExUnit] Allow moduledoc to be filtered/skipped in doctests
  * [ExUnit] Provide built-in log capturing functionality
  * [ExUnit] Allow `assert_receive_timeout` and `refute_receive_timeout` to be configured in the ExUnit application
  * [ExUnit] Allow tests to be skipped with `@tag :skip` or `@tag skip: "reason"`

#### IEx

  * [IEx] Support `IEx.pry` with `--remsh` for remote debugging
  * [IEx] Add `b/1` helper that shows documentation for behaviour modules and its callback functions
  * [IEx] Provide tab completion for aliases and allow aliases like `Foo.Bar.Baz` to autocomplete even if `Foo.Bar` is not defined

#### Logger

  * [Logger] Support printing pids and refs in Logger metadata
  * [Logger] Allow logger metadata to be removed from pdict by setting it to `nil`
  * [Logger] Add application configuration `translator_inspect_opts` for logger to customize how state and message are formatted when translating OTP errors and reports

#### Mix

  * [Mix] Check Elixir version right after archive installation and provide feedback if there is a mismatch
  * [Mix] Allow rebar dependencies with `mix.exs` to be compiled with Mix
  * [Mix] Allow rebar dependencies to be specified via `:path`
  * [Mix] Also consider subdirectories in `config` directory for `Mix.Project.config_files/0`
  * [Mix] Allow dynamic configuration in Mix projects by storing config in an agent
  * [Mix] Support rebar3 style git refs in `rebar.config` files
  * [Mix] Only recompile compile time dependencies in mix projects. This should considerably speed up recompilation times in Elixir projects
  * [Mix] Warn when configuring an application that is not available
  * [Mix] Add `mix profile.fprof` for easy code profiling

### 2. Bug fixes

#### Elixir

  * [CLI] Ensure Logger messages are flushed when executing commands
  * [Code] `:delegate_locals_to` failed to delegate to the chosen module in many situations and messed up stacktraces. This option has therefore been replaced by imports
  * [Exception] Do not fail when calculating an exception message, even if the message is invalid
  * [Kernel] Do not expand `in/2` argument in module body
  * [Kernel] Throw syntax error for undefind atom/alias syntax `:foo.Bar`
  * [Kernel] Improve error message when we can't compile because the target directory is not writeable
  * [Kernel] Allow capture of non-symbolic operators like `&and/2`, `&not/1` and others
  * [Module] Do not accept non-Elixir module names in `Module.split/1`
  * [Protocol] Guarantee that derived protocols go through `Any` instead of `Map`
  * [Range] Restrict ranges to integers to fix diverse bugs of values being included in the range when they should not (false positives)
  * [Regex] Fix splitting of empty strings with regexes when trim is set to `true`. Now both `String.split/3` and `Regex.split/3` return an empty list when called with an empty string and trim is enabled
  * [Regex] Fix `Regex.replace/4` so it doesn't discard escape characters

#### EEx

  * [EEx] Allow EEx interpolation to also apply inside quotations `<%%= ... %>`

#### ExUnit

  * [ExUnit] Skipped tests now correctly count towards the total of tests in the result returned by `ExUnit.run/0`
  * [ExUnit] Fix a bug where failures when inspecting data structure or retrieving error messages could bring the whole ExUnit runner down
  * [ExUnit] Do not change the semantics of evaluated code with `assert`/`refute`. For example, from now on, `assert nil = some_expr()` will now raise as expected, as the expression still evaluates to a falsy value

#### Logger

  * [Logger] Include metadata in `Logger.log/3`, use `Logger.bare_log/3` for runtime-only, with no metadata behaviour

#### Mix

  * [Mix] Use the safer `https` protocol instead of `git` for `:github` dependencies
  * [Mix] Ensure automatic protocol consolidation via `:consolidate_protocols` is triggered in umbrella apps
  * [Mix] Do not raise if wildcard given to `import_config` does not match any file
  * [Mix] Applications with `:build_embedded` set to true require explicit compilation step
  * [Mix] Also remove consolidated protocols on `mix clean`

### 3. Soft deprecations (no warnings emitted)

#### Elixir

  * [Behaviour] The module `Behaviour` is deprecated. Instead of `defcallback`, one can simply use `@callback`. Instead of `defmacrocallback`, one can simply use `@macrocallback`
  * [Enum] `Enum.uniq/2` is deprecated in favor of `Enum.uniq_by/2`
  * [Kernel] `\x` inside strings and charlists is deprecated in favor of `\uXXXX` and `\u{X*}`. The values emitted by `\x` are unfortunately wrong (they should be bytes but currently it emits codepoints). `\u` is meant to correctly map to codepoints and `\x` will be fixed in the future to map to bytes
  * [Regex] Ungreedy option `r` is deprecated in favor of `U` (which is standard in regular expressions in other languages)

### 4. Deprecations

#### Elixir

  * [Access] Implementing the Access protocol is deprecated. The Access protocol relies on the code server in development and test mode (when protocol consolidation is not applied) and it generated a bottleneck when working with multiple processes and the Access protocol was invoked hundreds of times (which is not uncommon). Note the `Access` module and the `opts[key]` syntax are not affected and they are not deprecated, only the underlying protocol dispatch
  * [Kernel] `?\xHEX` is deprecated in favor of `0xHEX`. There is no situation where the former should be used in favor of the latter and the latter is always cleaner
  * [Kernel] Giving `as: true | false` to `alias/2` and `require/2` have been deprecated (it was undocumented behaviour)
  * [String] Passing an empty string to `starts_with?`, `contains?` and `ends_with?` had dubious behaviour and have been deprecated to help developers identify possible bugs in their source code
