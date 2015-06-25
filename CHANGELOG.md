# Changelog for Elixir v1.1

v1.1 brings enhancements, bug fixes, performance improvements and more
into Elixir.

Elixir v1.1 supports both Erlang 17 and Erlang 18 and, for this reason,
it does not introduce any feature that is specific to Erlang 18. Such
will be tackled on the follow up Elixir v1.2 release.

On the enhancements side, the most notable changes are the new functions
added to `Enum` and `Dict` modules, and a new datatype called `MapSet`.
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

Note: Erlang 17.1 contains a regression in its wildcard implementation that
causes tools like rebar to fail. If you have a project with rebar dependencies
and is using Erlang 17.1, remember to update to at least Erlang 17.3.

## v1.1.0-dev

### 1 Enhancements

  * [Application] Add `Application.fetch_env!/2`
  * [CLI] Add support for `--werl` in Windows bash-like shells
  * [Dict] Add `Dict.get_and_update/3` which behaves similar to the now deprecated Access protocol
  * [Dict] Add `Dict.get_lazy/3`, `Dict.pop_lazy/3` and `Dict.put_new_lazy/3`
  * [EEx] Add `:trim` option to EEx that automatically trims the left side of `<%` and right side `%>` if only spaces and new lines preceed/follow them
  * [Enum] Add `Enum.random/1`, `Enum.minmax/1`, `Enum.minmax_by/2`, `Enum.reverse_slice/3`, `Enum.dedup/1` and `Enum.dedup_by/2`
  * [Enum] Inline common map usage in `Enum` functions for performance
  * [ExUnit] Add number of skipped tests to `ExUnit` output
  * [ExUnit] Make timeout configurable for the whole test suite via the `:timeout` configuration
  * [ExUnit] Allow moduledoc to be filtered/skipped in doctests
  * [File] Add `File.lstat/1` and `File.lstat/1` that works like `File.stat/1` but is able to return symlink information (i.e. it does not traverse symlinks)
  * [IEx] Add `b/1` helper that shows documentation for behaviour modules and its callback functions
  * [IEx] Provide tab completion for aliases and allow aliases like `Foo.Bar.Baz` to autocomplete even if `Foo.Bar` is not defined
  * [Integer] Add `Integer.digits/2` and `Integer.undigits/2`
  * [Inspect] Add the `:safe` option to `inspect/2` and make it safe by default, meaning failures while inspecting won't trigger other failures. Instead, it will be wrapped in an exception which is properly formatted
  * [IO] Support fenced code blocks on `IO.ANSI.Docs`
  * [Kernel] No longer include `:crypto` and `:syntax_tools` as dependencies. The former is only needed if you have encrypted debug info (therefore you can add `:crypto` as a dependency manually) and the latter is no longer used
  * [Kernel] Raise when `var.Alias` syntax is used and it does not expand to an atom at compile time (previously it just warned)
  * [Kernel] Improve generation of argument names for function signatures
  * [Kernel] `::/2` is now a special form
  * [Logger] Support printing pids and refs in Logger metadata
  * [Mix] Check Elixir version right after archive installation and provide feedback if there is a mismatch
  * [Mix] Allow rebar dependencies with `mix.exs` to be compiled with Mix
  * [Mix] Allow rebar dependencies to be specified via `:path`
  * [Mix] Also consider subdirectories in `config` directory for `Mix.Project.config_files/0`
  * [Mix] Allow dynamic configuration in Mix projects by storing config in an agent
  * [Module] Improve name inference for function signatures in documentation metadata
  * [Record] Expand attributes and macros when extracting records
  * [Set] Introduce `MapSet` data type. This new data type uses maps behind the scenes and is useful for storing a dozens of items in Erlang 17. In future versions when maps efficiently support large collections, it is meant to be the main Set abstraction in Elixir
  * [Stream] Add `Stream.dedup/1` and `Stream.dedup_by/2`
  * [String] Support calculation of the jaro distance between strings (usually names) via `String.jaro_distance/2`. This is used by Mix to support "Did you mean?" feature when a task does not exist
  * [String] Add `String.splitter/3` that splits strings as a stream
  * [StringIO] `StringIO.flush/1` was added to flush the output of a StringIO device
  * [URI] Default ports were added for "ws" and "wss" schemas

### 2. Bug fixes

  * [Code] `:delegate_locals_to` failed to delegate to the chosen module in many situations and messed up stacktraces. This option has therefore been replaced by imports
  * [EEx] Allow EEx interpolation to also apply inside quotations `<%%= ... %>`
  * [Exception] Do not fail when calculating an exception message, even if the message is invalid
  * [ExUnit] Skipped tests now correctly count towards the total of tests in the result returned by `ExUnit.run/0`
  * [ExUnit] Fix a bug where failures when inspecting data structure or retrieving error messages could bring the whole ExUnit runner down
  * [ExUnit] Ensure the Logger is flushed when running ExUnit via Mix
  * [Kernel] Do not expand `in/2` argument in module body
  * [Kernel] Throw syntax error for undefind atom/alias syntax `:foo.Bar`
  * [Kernel] Improve error message when we can't compile because the target directory is not writeable
  * [Mix] Ensure automatic protocol consolidation via `:consolidate_protocols` is triggered in umbrella apps
  * [Mix] Do not raise if wildcard given to `import_config` does not match any file
  * [Mix] Applications with build_embedded set to true require explicit compilation step
  * [Module] Do not accept non-Elixir module names in `Module.split/1`
  * [Protocol] Guarantee that derived protocols go through `Any` instead of `Map`
  * [Regex] Fix splitting of empty strings with regexes when trim is set to `true`. Now both `String.split/3` and `Regex.split/3` return an empty list when called with an empty string and trim is enabled
  * [Regex] Fix `Regex.replace/4` so it doesn't discard escape characters

### 3. Soft deprecations (no warnings emitted)

  * [Enum] `Enum.uniq/2` is deprecated in favor of `Enum.uniq_by/2`
  * [Regex] Ungreedy option `r` is deprecated in favor of `U` (which is standard in regular expressions in other languages)

### 4. Deprecations

  * [Access] Implementing the Access protocol is deprecated. The Access protocol relies on the code server in development and test mode (when protocol consolidation is not applied) and it generated a bottleneck when working with multiple processes and the Access protocol was invoked hundreds of times (which is not uncommon). Note the `Access` module and the `opts[key]` syntax are not affected and they are not deprecated, only the underlying protocol dispatch
  * [String] Passing an empty string to `starts_with?` and `ends_with?` had dubious behaviour and have been deprecated to help developers identify possible bugs in their source code
