# v0.13.0-dev

* Enhancements
  * [Base] Add `Base` module which does conversions to bases 16, 32, hex32, 64 and url64
  * [Code] Add `Code.eval_file/2`
  * [Collectable] Add the `Collectable` protocol that empowers `Enum.into/2` and `Stream.into/2` and the `:into` option in comprehensions
  * [Collectable] Implement `Collectable` for lists, dicts, bitstrings, functions and provide both `File.Stream` and `IO.Stream`
  * [Enum] Add `Enum.group_by/2`, `Enum.into/2`, `Enum.into/3`, `Enum.traverse/2` and `Enum.sum/2`
  * [ExUnit] Randomize cases and tests suite runs, allow seed configuration and the `--seed` flag via `mix test`
  * [ExUnit] Support `--only` for filtering when running tests with `mix test`
  * [ExUnit] Raise an error if another `capture_io` process already captured the device
  * [IEx] Allow prompt configuration with the `:prompt` option
  * [Kernel] Support `ERL_PATH` in `bin/elixir`
  * [Kernel] Support interpolation in keyword syntax
  * [Map] Add a Map module and support 17.0 maps and structs
  * [Mix] Add dependency option `:only` to specify the dependency environment. `mix deps.get` and `mix deps.update` works accross all environment unless `--only` is specified
  * [Mix] Add `Mix.Shell.prompt/1`
  * [Mix] Ensure the project is compiled in case Mix' CLI cannot find a task
  * [Node] Add `Node.ping/1`
  * [Process] Include `Process.send/3` and support the `--gen-debug` option
  * [Regex] Regexes no longer need the "g" option when there is a need to use named captures
  * [Stream] Add `Stream.into/2` and `Stream.into/3`
  * [StringIO] Add a `StringIO` module that allows a String to be used as IO device
  * [System] Add `System.delete_env/1` to remove a variable from the environment

* Bug fixes
  * [Kernel] Ensure the same pid is not queued twice in the parallel compiler
  * [Macro] `Macro.to_string/2` considers proper precedence when translating `!(foo > bar)` into a string
  * [Mix] Automatically recompile on outdated Elixir version and show proper error messages
  * [Mix] Ensure generated `.app` file includes core dependencies
  * [OptionParser] Do not recognize undefined aliases as switches

* Deprecations
  * [Dict] `Dict.empty/1`, `Dict.new/1` and `Dict.new/2` are deprecated
  * [Exception] `Exception.normalize/1` is deprecated in favor of `Exception.normalize/2`
  * [Kernel] `lc` and `bc` comprehensions are deprecated in favor of `for` (this is a soft deprecation, no warning will be emitted)
  * [ListDict] `ListDict` is deprecated in favor of `Map` (this is a soft deprecation, no warning will be emitted)
  * [Record] `defrecord/2`, `defrecordp/3`, `is_record/1` and `is_record/2` macros in Kernel are deprecated. Instead, use the new macros and API defined in the `Record` module (this is a soft deprecation, no warnings will be emitted)

* Backwards incompatible changes
  * [ExUnit] Formatters are now required to be a GenEvent and `ExUnit.run/2` returns a map with results

# v0.12.5 (2014-03-09)

* Bug fixes
  * [Kernel] Ensure `try` does not generate an after clause. Generating an after clause forbade clauses in the `else` part from being tail recursive. This should improve performance and memory consumption of `Stream` functions
  * [Mix] Automatically recompile on outdated Elixir version and show proper error messages

* Deprecations
  * [File] `File.stream_to!/3` is deprecated
  * [GenFSM] `GenFSM` is deprecated
  * [Kernel] `%` for sigils is deprecated in favor of `~`
  * [Kernel] `is_range/1` and `is_regex/1` are deprecated in favor of `Range.range?/1` and `Regex.regex?/1`
  * [Stream] `Stream.after/1` is deprecated
  * [URI] `URI.decode_query/1` is deprecated in favor of `URI.decode_query/2` with explicit dict argument
  * [URI] Passing lists as key or values in `URI.encode_query/1` is deprecated

* Backwards incompatible changes
  * [Mix] Remove `MIX_GIT_FORCE_HTTPS` as Git itself already provides mechanisms for doing so

# v0.12.4 (2014-02-12)

* Enhancements
  * [Mix] `mix deps.get` and `mix deps.update` no longer compile dependencies afterwards. Instead, they mark the dependencies which are going to be automatically compiled next time `deps.check` is invoked (which is done automatically by most mix tasks). This means users should have a better workflow when migrating in between environments

* Deprecations
  * [Kernel] `//` for default arguments is deprecated in favor of `\\`
  * [Kernel] Using `%` for sigils is deprecated in favor of `~`. This is a soft deprecation, no warnings will be emitted for it in this release
  * [Kernel] Using `^` inside function clause heads is deprecated, please use a guard instead

* Backwards incompatible changes
  * [ExUnit] `CaptureIO` returns an empty string instead of nil when there is no capture
  * [Version] The `Version` module now only works with SemVer. The functions `Version.parse/1` and `Version.parse_requirement/1` now return `{:ok,res} | :error` for the cases you want to handle non SemVer cases manually. All other functions will trigger errors on non semantics versions

# v0.12.3 (2014-02-02)

* Enhancements
  * [Kernel] Warnings now are explicitly tagged with "warning:" in messages
  * [Kernel] Explicit functions inlined by the compiler, including operators. This means that `Kernel.+/2` will now expand to `:erlang.+/2` and so on
  * [Mix] Do not fail if a Mix dependency relies on an outdated Elixir version
  * [Process] Add `Process.send/2` and `Process.send_after/3`
  * [Version] Add `Version.compare/2`

* Bug fixes
  * [Atom] Inspect `:...` and `:foo@bar` without quoting
  * [Keyword] The list `[1, 2, three: :four]` now correctly expands to `[1, 2, {:three, :four}]`
  * [Kernel] Ensure undefined `@attributes` shows proper stacktrace in warnings
  * [Kernel] Guarantee nullary funs/macros are allowed in guards
  * [Process] Ensure monitoring functions are inlined by the compiler

* Deprecations
  * [IEx] The helper `m/0` has been deprecated. The goal is to group all runtime statistic related helpers into a single module
  * [Kernel] `binary_to_term/1`, `binary_to_term/2`, `term_to_binary/1` and `term_to_binary/2` are deprecated in favor of their counterparts in the `:erlang` module
  * [Kernel] `//` for default arguments is deprecated in favor of `\\`. This is a soft deprecation, no warnings will be emitted for it in this release
  * [Kernel] Deprecated `@behavior` in favor of `@behaviour`
  * [Record] `to_keywords`, `getter` and `list getter` functionalities in `defrecordp` are deprecated
  * [Record] `Record.import/2` is deprecated

* Backwards incompatible changes
  * [Dict] Implementations of `equal?/2` and `merge/2` in `HashDict` and `ListDict` are no longer polymorphic. To get polymorphism, use the functions in `Dict` instead
  * [File] `File.cp/3` and `File.cp_r/3` no longer carry Unix semantics where the function behaves differently if the destination is an existing previous directory or not. It now always copies source to destination, doing it recursively in the latter
  * [IEx] IEx now loads the `.iex.exs` file instead of `.iex`
  * [Kernel] Remove `**` from the list of allowed operators
  * [Kernel] Limit sigils delimiters to one of the following: `<>`, `{}`, `[]`, `()`, `||`, `//`, `"` and `'`
  * [Range] `Range` is no longer a record, instead use `first .. last` if you need pattern matching
  * [Set] Implementations of `difference/2`, `disjoint?/2`, `equal?/2`, `intersection/2`, `subset?/2` and `union/2` in `HashSet` are no longer polymorphic. To get polymorphism, use the functions in `Set` instead

# v0.12.2 (2014-01-15)

* Enhancements
  * [EEx] Allow `EEx.AssignsEngine` to accept any Dict
  * [Enum] Add `Enum.flat_map_reduce/3`
  * [ExUnit] Support `@moduletag` in ExUnit cases
  * [Kernel] Improve stacktraces to be relative to the compilation path and include the related application
  * [Stream] Add `Stream.transform/3`

* Bug fixes
  * [ExUnit] `:include` in ExUnit only has effect if a test was previously excluded with `:exclude`
  * [ExUnit] Only run `setup_all` and `teardown_all` if there are tests in the case
  * [Kernel] Ensure bitstring modifier arguments are expanded
  * [Kernel] Ensure compiler does not block on missing modules
  * [Kernel] Ensure `<>/2` works only with binaries
  * [Kernel] Fix usage of string literals inside `<<>>` when `utf8`/`utf16`/`utf32` is used as specifier
  * [Mix] Ensure mix properly copies _build dependencies on Windows

* Deprecations
  * [Enum] Deprecate `Enum.first/1` in favor of `Enum.at/2` and `List.first/1`
  * [Kernel] Deprecate continuable heredocs. In previous versions, Elixir would continue parsing on the same line the heredoc started, this behaviour has been deprecated
  * [Kernel] `is_alive/0` is deprecated in favor of `Node.alive?`
  * [Kernel] `Kernel.inspect/2` with `Inspect.Opts[]` is deprecated in favor of `Inspect.Algebra.to_doc/2`
  * [Kernel] `Kernel.inspect/2` with `:raw` option is deprecated, use `:records` option instead
  * [Kernel] Deprecate `<-/2` in favor of `send/2`

* Backwards incompatible changes
  * [String] Change `String.next_grapheme/1` and `String.next_codepoint/1` to return `nil` on string end

# v0.12.1 (2014-01-04)

* Enhancements
  * [ExUnit] Support `:include` and `:exclude` configuration options to filter which tests should run based on their tags. Those options are also supported via `mix test` as `--include` and `--exclude`
  * [ExUnit] Allow doctests to match against `#MyModule<>`

* Bug fixes
  * [CLI] Abort when a pattern given to elixirc does not match any file
  * [Float] Fix `Float.parse/1` to handle numbers of the form "-0.x"
  * [IEx] Improve error message for `IEx.Helpers.r` when module does not exist
  * [Mix] Ensure `deps.get` updates origin if lock origin and dep origin do not match
  * [Mix] Use relative symlinks in _build
  * [Typespec] Fix conversion of unary ops from typespec format to ast
  * [Typespec] Fix handling of `tuple()` and `{}`

* Deprecations
  * [Kernel] Do not leak clause heads. Previously, a variable defined in a case/receive head clauses would leak to the outer scope. This behaviour is deprecated and will be removed in the next release.
  * [Kernel] Deprecate `__FILE__` in favor of `__DIR__` or `__ENV__.file`

* Backwards incompatible changes
  * [GenFSM] GenServer now stops on unknown event/sync_event requests
  * [GenServer] GenServer now stops on unknown call/cast requests
  * [Kernel] Change how `->` is represented in AST. Now each clause is represented by its own AST node which makes composition easier. See commit 51aef55 for more information.

# v0.12.0 (2013-12-15)

* Enhancements
  * [Exception] Allow `exception/1` to be overridden and promote it as the main mechanism to customize exceptions
  * [File] Add `File.stream_to!/3`
  * [Float] Add `Float.floor/1`, `Float.ceil/1` and `Float.round/3`
  * [Kernel] Add `List.delete_at/2` and `List.updated_at/3`
  * [Kernel] Add `Enum.reverse/2`
  * [Kernel] Implement `defmodule/2`, `@/1`, `def/2` and friends in Elixir itself. `case/2`, `try/2` and `receive/1` have been made special forms. `var!/1`, `var!/2` and `alias!/1` have also been implemented in Elixir and demoted from special forms
  * [Record] Support dynamic fields in `defrecordp`
  * [Stream] Add `Stream.resource/3`
  * [Stream] Add `Stream.zip/2`, `Stream.filter_map/3`, `Stream.each/2`, `Stream.take_every/2`, `Stream.chunk/2`, `Stream.chunk/3`, `Stream.chunk/4`, `Stream.chunk_by/2`, `Stream.scan/2`, `Stream.scan/3`, `Stream.uniq/2`, `Stream.after/2` and `Stream.run/1`
  * [Stream] Support `Stream.take/2` and `Stream.drop/2` with negative counts

* Bug fixes
  * [HashDict] Ensure a `HashDict` stored in an attribute can be accessed via the attribute
  * [Enum] Fix bug in `Enum.chunk/4` where you'd get an extra element when the enumerable was a multiple of the counter and a pad was given
  * [IEx] Ensure `c/2` helper works with full paths
  * [Kernel] `quote location: :keep` now only affects definitions in order to keep the proper trace in definition exceptions
  * [Mix] Also symlink `include` directories in _build dependencies
  * [Version] Fix `Version.match?/2` with `~>` and versions with alphanumeric build info (like `-dev`)

* Deprecations
  * [Enum] `Enumerable.count/1` and `Enumerable.member?/2` should now return tagged tuples. Please see `Enumerable` docs for more info
  * [Enum] Deprecate `Enum.chunks/2`, `Enum.chunks/4` and `Enum.chunks_by/2` in favor of `Enum.chunk/2`, `Enum.chunk/4` and `Enum.chunk_by/2`
  * [File] `File.binstream!/3` is deprecated. Simply use `File.stream!/3` which is able to figure out if `stream` or `binstream` operations should be used
  * [Macro] `Macro.extract_args/1` is deprecated in favor of `Macro.decompose_call/1`

* Backwards incompatible changes
  * [Enum] Behaviour of `Enum.drop/2` and `Enum.take/2` has been switched when given negative counts
  * [Enum] Behaviour of `Enum.zip/2` has been changed to stop as soon as the first enumerable finishes
  * [Enum] `Enumerable.reduce/3` protocol has changed to support suspension. Please see `Enumerable` docs for more info
  * [Mix] Require `:escript_main_module` to be set before generating escripts
  * [Range] `Range.Iterator` protocol has changed in order to work with the new `Enumerable.reduce/3`. Please see `Range.Iterator` docs for more info
  * [Stream] The `Stream.Lazy` structure has changed to accumulate functions and accumulators as we go (its inspected representation has also changed)
  * [Typespec] `when` clauses were moved to the outer part of the spec and should be in the keywords format. So `add(a, b) when is_subtype(a, integer) and is_subtype(b, integer) :: integer` should now be written as `add(a, b) :: integer when a: integer, b: integer`

# v0.11.2 (2013-11-14)

* Enhancements
  * [Mix] Add `mix iex` that redirects users to the proper `iex -S mix` command
  * [Mix] Support `build_per_environment: true` in project configuration that manages a separete build per environment, useful when you have per-environment behaviour/compilation

* Backwards incompatible changes
  * [Mix] Mix now compiles files to `_build`. Projects should update just fine, however documentation and books may want to update to the latest information

# v0.11.1 (2013-11-07)

* Enhancements
  * [Mix] Improve dependency convergence by explicitly checking each requirement instead of expecting all requirements to be equal
  * [Mix] Support optional dependencies with `optional: true`. Optional dependencies are downloaded for the current project but they are automatically skipped when such project is used as a dependency

* Bug fixes
  * [Kernel] Set compilation status per ParallelCompiler and not globally
  * [Mix] Ensure Mix does not load previous dependencies versions before `deps.get`/`deps.update`
  * [Mix] Ensure umbrella apps are sorted before running recursive commands
  * [Mix] Ensure umbrella apps run in the same environment as the parent project
  * [Mix] Ensure dependency tree is topsorted before compiling
  * [Mix] Raise error when duplicated projects are pushed into the stack
  * [URI] Allow lowercase escapes in URI

* Backwards incompatible changes
  * [Mix] Setting `:load_paths` in your project configuration is deprecated

# v0.11.0 (2013-11-02)

* Enhancements
  * [Code] Eval now returns variables from other contexts
  * [Dict] Document and enforce all dicts use the match operator (`===`) when checking for keys
  * [Enum] Add `Enum.slice/2` with a range
  * [Enum] Document and enforce `Enum.member?/2` to use the match operator (`===`)
  * [IEx] Split `IEx.Evaluator` from `IEx.Server` to allow custom evaluators
  * [IEx] Add support for `IEx.pry` which halts a given process for inspection
  * [IO] Add specs and allow some IO APIs to receive any data that implements `String.Chars`
  * [Kernel] Improve stacktraces on command line interfaces
  * [Kernel] Sigils can now handle balanced tokens as in `%s(f(o)o)`
  * [Kernel] Emit warnings when an alias is not used
  * [Macro] Add `Macro.pipe/3` and `Macro.unpipe/1` for building pipelines
  * [Mix] Allow umbrella children to share dependencies between them
  * [Mix] Allow mix to be escriptize'd
  * [Mix] Speed mix projects compilation by relying on more manifests information
  * [Protocol] Protocols now provide `impl_for/1` and `impl_for!/1` functions which receive a structure and returns its respective implementation, otherwise returns nil or an error
  * [Set] Document and enforce all sets use the match operator (`===`) when checking for keys
  * [String] Update to Unicode 6.3.0
  * [String] Add `String.slice/2` with a range

* Bug fixes
  * [Exception] Ensure `defexception` fields can be set dynamically
  * [Kernel] Guarantee aliases hygiene is respected when the current module name is not known upfront
  * [Kernel] `Kernel.access/2` no longer flattens lists
  * [Mix] Ensure cyclic dependencies are properly handled
  * [String] Implement the extended grapheme cluster algorithm for `String` operations

* Deprecations
  * [Kernel] `pid_to_list/1`, `list_to_pid/1`, `binary_to_atom/2`, `binary_to_existing_atom/2` and `atom_to_binary/2` are deprecated in favor of their counterparts in the `:erlang` module
  * [Kernel] `insert_elem/3` and `delete_elem/2` are deprecated in favor of `Tuple.insert_at/3` and `Tuple.delete_at/2`
  * [Kernel] Use of `in` inside matches (as in `x in [1,2,3] -> x`) is deprecated in favor of the guard syntax (`x when x in [1,2,3]`)
  * [Macro] `Macro.expand_all/2` is deprecated
  * [Protocol] `@only` and `@except` in protocols are now deprecated
  * [Protocol] Protocols no longer fallback to `Any` out of the box (this functionality needs to be explicitly enabled by setting `@fallback_to_any` to true)
  * [String] `String.to_integer/1` and `String.to_float/1` are deprecated in favor of `Integer.parse/1` and `Float.parse/1`

* Backwards incompatible changes
  * [CLI] Reading `.elixirrc` has been dropped in favor of setting env vars
  * [Kernel] `Kernel.access/2` now expects the second argument to be a compile time list
  * [Kernel] `fn -> end` quoted expression is no longer wrapped in a `do` keyword
  * [Kernel] Quoted variables from the same module must be explicitly shared. Previously, if a function returned `quote do: a = 1`, another function from the same module could access it as `quote do: a`. This has been fixed and the variables must be explicitly shared with `var!(a, __MODULE__)`
  * [Mix] Umbrella apps now treat children apps as dependencies. This means all dependencies will be checked out in the umbrela `deps` directory. On upgrade, child apps need to point to the umbrella project by setting `deps_path: "../../deps_path", lockfile: "../../mix.lock"` in their project config
  * [Process] `Process.group_leader/2` args have been reversed so the "subject" comes first
  * [Protocol] Protocol no longer dispatches to `Number`, but to `Integer` and `Float`

# v0.10.3 (2013-10-02)

* Enhancements
  * [Enum] Add `Enum.take_every/2`
  * [IEx] IEx now respects signals sent from the Ctrl+G menu
  * [Kernel] Allow documentation for types with `@typedoc`
  * [Mix] Allow apps to be selected in umbrella projects
  * [Record] Generated record functions `new` and `update` also take options with strings as keys
  * [Stream] Add `Stream.unfold/1`

* Bug fixes
  * [Dict] Fix a bug when a HashDict was marked as equal when one was actually a subset of the other
  * [EEx] Solve issue where `do` blocks inside templates were not properly aligned
  * [ExUnit] Improve checks and have better error reports on poorly aligned doctests
  * [Kernel] Fix handling of multiple heredocs on the same line
  * [Kernel] Provide better error messages for match, guard and quoting errors
  * [Kernel] Make `Kernel.raise/2` a macro to avoid messing up stacktraces
  * [Kernel] Ensure `&()` works on quoted blocks with only one expression
  * [Mix] Address an issue where a dependency was not compiled in the proper order when specified in different projects
  * [Mix] Ensure `compile: false` is a valid mechanism for disabling the compilation of dependencies
  * [Regex] Fix bug on `Regex.scan/3` when capturing groups and the regex has no groups
  * [String] Fix a bug with `String.split/2` when given an empty pattern
  * [Typespec] Guarantee typespecs error reports point to the proper line

* Deprecations
  * [Kernel] The previous partial application syntax (without the `&` operator) has now been deprecated
  * [Regex] `Regex.captures/3` is deprecated in favor of `Regex.named_captures/3`
  * [String] `String.valid_codepoint?/1` is deprecated in favor of pattern matching with `<<_ :: utf8 >>`

* Backwards incompatible changes
  * [IEx] The `r/0` helper has been removed as it caused surprising behaviour when many modules with dependencies were accumulated
  * [Mix] `Mix.Version` was renamed to `Version`
  * [Mix] `File.IteratorError` was renamed to `IO.StreamError`
  * [Mix] `mix new` now defaults to the `--sup` option, use `--bare` to get the previous behaviour

# v0.10.2 (2013-09-03)

* Enhancements
  * [CLI] Add `--verbose` to elixirc, which now is non-verbose by default
  * [Dict] Add `Dict.Behaviour` as a convenience to create your own dictionaries
  * [Enum] Add `Enum.split/2`, `Enum.reduce/2`, `Enum.flat_map/2`, `Enum.chunk/2`, `Enum.chunk/4`, `Enum.chunk_by/2`, `Enum.concat/1` and `Enum.concat/2`
  * [Enum] Support negative indices in `Enum.at/fetch/fetch!`
  * [ExUnit] Show failures on CLIFormatter as soon as they pop up
  * [IEx] Allow for strings in `h` helper
  * [IEx] Helpers `r` and `c` can handle erlang sources
  * [Integer] Add `odd?/1` and `even?/1`
  * [IO] Added support to specifying a number of bytes to stream to `IO.stream`, `IO.binstream`, `File.stream!` and `File.binstream!`
  * [Kernel] Include file and line on error report for overriding an existing function/macro
  * [Kernel] Convert external functions into quoted expressions. This allows record fields to contain functions as long as they point to an `&Mod.fun/arity`
  * [Kernel] Allow `foo?` and `bar!` as valid variable names
  * [List] Add `List.replace_at/3`
  * [Macro] Improve printing of the access protocol on `Macro.to_string/1`
  * [Macro] Add `Macro.to_string/2` to support annotations on the converted string
  * [Mix] Automatically recompile a project if the Elixir version changes
  * [Path] Add `Path.relative_to_cwd/2`
  * [Regex] Allow erlang `re` options when compiling Elixir regexes
  * [Stream] Add `Stream.concat/1`, `Stream.concat/2` and `Stream.flat_map/2`
  * [String] Add regex pattern support to `String.replace/3`
  * [String] Add `String.ljust/2`, `String.rjust/2`, `String.ljust/3` and `String.rjust/3`
  * [URI] `URI.parse/1` supports IPv6 addresses

* Bug fixes
  * [Behaviour] Do not compile behaviour docs if docs are disabled on compilation
  * [ExUnit] Doctests no longer eat too much space and provides detailed reports for poorly indented lines
  * [File] Fix a bug where `File.touch(file, datetime)` was not setting the proper datetime when the file did not exist
  * [Kernel] Limit `inspect` results to 50 items by default to avoid printing too much data
  * [Kernel] Return a readable error on oversized atoms
  * [Kernel] Allow functions ending with `?` or `!` to be captured
  * [Kernel] Fix default shutdown of child supervisors to `:infinity`
  * [Kernel] Fix regression when calling a function/macro ending with bang, followed by `do/end` blocks
  * [List] Fix bug on `List.insert_at/3` that added the item at the wrong position for negative indexes
  * [Macro] `Macro.escape/2` can now escape improper lists
  * [Mix] Fix `Mix.Version` matching on pre-release info
  * [Mix] Ensure `watch_exts` trigger full recompilation on change with `mix compile`
  * [Mix] Fix regression on `mix clean --all`
  * [String] `String.strip/2` now supports removing unicode characters
  * [String] `String.slice/3` still returns the proper result when there is no length to be extracted
  * [System] `System.get_env/0` now returns a list of tuples as previously advertised

* Deprecations
  * [Dict] `Dict.update/3` is deprecated in favor of `Dict.update!/3`
  * [Enum] `Enum.min/2` and `Enum.max/2` are deprecated in favor of `Enum.min_by/2` and `Enum.max_by/2`
  * [Enum] `Enum.join/2` and `Enum.map_join/3` with a char list are deprecated
  * [IO] `IO.stream(device)` and `IO.binstream(device)` are deprecated in favor of `IO.stream(device, :line)` and `IO.binstream(device, :line)`
  * [Kernel] `list_to_binary/1`, `binary_to_list/1` and `binary_to_list/3` are deprecated in favor of `String.from_char_list!/1` and `String.to_char_list!/1` for characters and `:binary.list_to_bin/1`, `:binary.bin_to_list/1` and `:binary.bin_to_list/3` for bytes
  * [Kernel] `to_binary/1` is deprecated in favor of `to_string/1`
  * [Kernel] Deprecate `def/4` and friends in favor of `def/2` with unquote and friends
  * [Kernel] Deprecate `%b` and `%B` in favor of `%s` and `%S`
  * [List] `List.concat/2` is deprecated in favor of `Enum.concat/2`
  * [Macro] `Macro.unescape_binary/1` and `Macro.unescape_binary/2` are deprecated in favor of `Macro.unescape_string/1` and `Macro.unescape_string/2`
  * [Mix] `:umbrella` option for umbrella paths has been deprecated in favor of `:in_umbrella`

* Backwards incompatible changes
  * [IO] IO functions now only accept iolists as arguments
  * [Kernel] `Binary.Chars` was renamed to `String.Chars`
  * [Kernel] The previous ambiguous import syntax `import :functions, Foo` was removed in favor of `import Foo, only: :functions`
  * [OptionParser] `parse` and `parse_head` now returns a tuple with three elements instead of two

# v0.10.1 (2013-08-03)

* Enhancements
  * [Behaviour] Add support for `defmacrocallback/1`
  * [Enum] Add `Enum.shuffle/1`
  * [ExUnit] The `:trace` option now also reports run time for each test
  * [ExUnit] Add support for `:color` to enable/disable ANSI coloring
  * [IEx] Add the `clear` helper to clear the screen.
  * [Kernel] Add the capture operator `&`
  * [Kernel] Add support for `GenFSM.Behaviour`
  * [Kernel] Functions now points to the module and function they were defined when inspected
  * [Kernel] A documentation attached to a function that is never defined now prints warnings
  * [List] Add `List.keysort/2`
  * [Mix] `:test_helper` project configuration did not affect `mix test` and was therefore removed. A `test/test_helper.exs` file is still necessary albeit it doesn't need to be automatically required in each test file
  * [Mix] Add manifests for yecc, leex and Erlang compilers, making it easier to detect dependencies in between compilers and providing a more useful clean behaviour
  * [Mix] `mix help` now outputs information about the default mix task
  * [Mix] Add `--no-deps-check` option to `mix run`, `mix compile` and friends to not check dependency status
  * [Mix] Add support for `MIX_GIT_FORCE_HTTPS` system environment that forces HTTPS for known providers, useful when the regular git port is blocked. This configuration does not affect the `mix.lock` results
  * [Mix] Allow coverage tool to be pluggable via the `:test_coverage` configuration
  * [Mix] Add `mix cmd` as a convenience to run a command recursively in child apps in an umbrella application
  * [Mix] Support `umbrella: true` in dependencies as a convenience for setting up umbrella path deps
  * [Mix] `mix run` now behaves closer to the `elixir` command and properly mangles the ARGV
  * [String] Add `Regex.scan/3` now supports capturing groups
  * [String] Add `String.reverse/1`

* Bug fixes
  * [Behaviour] Ensure callbacks are stored in the definition order
  * [CLI] Speed up boot time on Elixir .bat files
  * [IEx] Reduce cases where IEx parser can get stuck
  * [Kernel] Improve error messages when the use of an operator has no effect
  * [Kernel] Fix a bug where warnings were not being generated when imported macros conflicted with local functions or macros
  * [Kernel] Document that `on_definition` can only be a function as it is evaluated inside the function context
  * [Kernel] Ensure `%w` sigils with no interpolation are fully expanded at compile time
  * [Mix] `mix deps.update`, `mix deps.clean` and `mix deps.unlock` no longer change all dependencies unless `--all` is given
  * [Mix] Always run ` mix loadpaths` on `mix app.start`, even if `--no-compile` is given
  * [OptionParser] Do not add boolean flags to the end result if they were not given
  * [OptionParser] Do not parse non-boolean flags as booleans when true or false are given
  * [OptionParser] Ensure `:keep` and `:integer`|`:float` can be given together as options
  * [OptionParser] Ensure `--no-flag` sets `:flag` to false when `:flag` is a registered boolean switch

* Deprecations
  * [Kernel] `function(Mod.fun/arity)` and `function(fun/arity)` are deprecated in favor of `&Mod.fun/arity` and `&fun/arity`
  * [Kernel] `function/3` is deprecated in favor of `Module.function/3`
  * [Kernel] `Kernel.ParallelCompiler` now receives a set of callbacks instead of a single one
  * [Mix] `:test_coverage` option now expect keywords arguments and the `--cover` flag is now treated as a boolean

* Backwards incompatible changes
  * [Regex] `Regex.scan/3` now always returns a list of lists, normalizing the result, instead of list with mixed lists and binaries
  * [System] `System.halt/2` was removed since the current Erlang implementation of such function is bugged

# v0.10.0 (2013-07-15)

* Enhancements
  * [ExUnit] Support `trace: true` option which gives detailed reporting on test runs
  * [HashDict] Optimize `HashDict` to store pairs in a cons cell reducing storage per key by half
  * [Kernel] Add pretty printing support for inspect
  * [Kernel] Add document algebra library used as the foundation for pretty printing
  * [Kernel] Add `defrecordp/3` that enables specifying the first element of the tuple
  * [Kernel] Add the `Set` API and a hash based implementation via `HashSet`
  * [Kernel] Add `Stream` as composable, lazy-enumerables
  * [Mix] `mix archive` now includes the version of the generated archive
  * [Mix] Mix now requires explicit dependency overriding to be given with `override: true`
  * [Mix] Projects can now define an `:elixir` key to outline supported Elixir versions
  * [Typespec] Improve error messages to contain file, line and the typespec itself

* Bug fixes
  * [CLI] Elixir can now run on Unix directories with `:` in its path
  * [Kernel] `match?/2` does not leak variables to outer scope
  * [Kernel] Keep `head|tail` format when splicing at the tail
  * [Kernel] Ensure variables defined in the module body are not passed to callbacks
  * [Mix] On dependencies conflict, show from where each source is coming from
  * [Mix] Empty projects no longer leave empty ebin files on `mix compile`
  * [Module] Calling `Module.register_attribute/3` no longer automatically changes it to persisted or accumulated

* Deprecations
  * [Enum] Receiving the index of iteration in `Enum.map/2` and `Enum.each/2` is deprecated in favor of `Stream.with_index/1`
  * [File] `File.iterator/1` and `File.biniterator/1` are deprecated in favor of `IO.stream/1` and `IO.binstream/1`
  * [File] `File.iterator!/2` and `File.biniterator!/2` are deprecated in favor of `File.stream!/2` and `File.binstream!/2`
  * [Kernel] Deprecate recently added `quote binding: ...` in favor of the clearer `quote bind_quoted: ...`
  * [Kernel] Deprecate `Kernel.float/1` in favor of a explicit conversion
  * [Mix] Deprecate `mix run EXPR` in favor of `mix run -e EXPR`
  * [Record] `Record.__index__/2` deprecated in favor of `Record.__record__(:index, key)`

* Backwards incompatible changes
  * [Kernel] The `Binary.Inspect` protocol has been renamed to `Inspect`
  * [Kernel] Tighten up the grammar rules regarding parentheses omission, previously the examples below would compile but now they raise an error message:

            do_something 1, is_list [], 3
            [1, is_atom :foo, 3]

  * [Module] Calling `Module.register_attribute/3` no longer automatically changes it to persisted or accumulated
  * [Record] First element of a record via `defrecordp` is now the `defrecordp` name and no longer the current atom
  * [URI] Remove custom URI parsers in favor of `URI.default_port/2`

# v0.9.3 (2013-06-23)

* Enhancements
  * [File] Add `File.chgrp`, `File.chmod` and `File.chown`
  * [Kernel] Add `--warnings-as-errors` to Elixir's compiler options
  * [Kernel] Print warnings to stderr
  * [Kernel] Warn on undefined module attributes
  * [Kernel] Emit warning for `x in []` in guards
  * [Kernel] Add `binding/0` and `binding/1` for retrieving bindings
  * [Kernel] `quote` now allows a binding as an option
  * [Macro] Add `Macro.expand_once/2` and `Macro.expand_all/2`
  * [Mix] Implement `Mix.Version` for basic versioning semantics
  * [Mix] Support creation and installation of archives (.ez files)
  * [Mix] `github: ...` shortcut now uses the faster `git` schema instead of `https`
  * [Record] Allow types to be given to `defrecordp`

* Bug fixes
  * [Kernel] The elixir executable on Windows now supports the same options as the UNIX one
  * [Kernel] Improve error messages on default clauses clash
  * [Kernel] `__MODULE__.Foo` now returns `Foo` when outside of a Module
  * [Kernel] Improve error messages when default clauses from different definitions collide
  * [Kernel] `^x` variables should always refer to the value before the expression
  * [Kernel] Allow `(x, y) when z` in function clauses and try expressions
  * [Mix] Mix now properly evaluates rebar scripts

* Deprecations
  * [Code] `Code.string_to_ast/1` has been deprecated in favor of `Code.string_to_quoted/1`
  * [Macro] `Macro.to_binary/1` has been deprecated in favor of `Macro.to_string/1`
  * [Typespec] Deprecate `(fun(...) -> ...)` in favor of `(... -> ...)`

* Backwards incompatible changes
  * [Bitwise] Precedence of operators used by the Bitwise module were changed, check `elixir_parser.yrl` for more information
  * [File] `rm_rf` and `cp_r` now returns a tuple with three elements on failures
  * [Kernel] The quoted representation for `->` clauses changed from a tuple with two elements to a tuple with three elements to support metadata
  * [Kernel] Sigils now dispatch to `sigil_$` instead of `__$__` where `$` is the sigil character
  * [Macro] `Macro.expand/2` now expands until final form. Although this is backwards incompatible, it is very likely you do not need to change your code, since expansion until its final form is recommended, particularly if you are expecting an atom out of it
  * [Mix] No longer support beam files on `mix local`

# v0.9.2 (2013-06-13)

* Enhancements
  * [ExUnit] `capture_io` now captures prompt by default
  * [Mix] Automatically import git dependencies from Rebar
  * [Mix] Support for dependencies directly from the umbrella application
  * [Regex] Add `Regex.escape`
  * [String] Add `String.contains?`
  * [URI] Implement `Binary.Chars` (aka `to_binary`) for `URI.Info`

* Bug fixes
  * [HashDict] Ensure HashDict uses exact match throughout its implementation
  * [IEx] Do not interpret ANSI codes in IEx results
  * [IEx] Ensure `--cookie` is set before accessing remote shell
  * [Kernel] Do not ignore nil when dispatching protocols to avoid infinite loops
  * [Mix] Fix usage of shell expressions in `Mix.Shell.cmd`
  * [Mix] Start the application by default on escripts

* Deprecations
  * [Regex] `Regex.index/2` is deprecated in favor `Regex.run/3`
  * [Kernel] `super` no longer supports implicit arguments

* Backwards incompatible changes
  * [Kernel] The `=~` operator now returns true or false instead of an index

# v0.9.1 (2013-05-30)

* Enhancements
  * [IEx] Limit the number of entries kept in history and allow it to be configured
  * [Kernel] Add `String.start_with?` and `String.end_with?`
  * [Typespec] Allow keywords, e.g. `[foo: integer, bar: boolean | module]`, in typespecs

* Bug fixes
  * [Dict] `Enum.to_list` and `Dict.to_list` now return the same results for dicts
  * [IEx] Enable shell customization via the `IEx.Options` module
  * [Kernel] Fix a bug where `unquote_splicing` did not work on the left side of a stab op
  * [Kernel] Unused functions with cyclic dependencies are now also warned as unused
  * [Mix] Fix a bug where `mix deps.get` was not retrieving nested dependencies
  * [Record] Fix a bug where nested records cannot be defined
  * [Record] Fix a bug where a record named Record cannot be defined

# v0.9.0 (2013-05-23)

* Enhancements
  * [ExUnit] `ExUnit.CaptureIO` now accepts an input to be used during capture
  * [IEx] Add support for .iex files that are loaded during shell's boot process
  * [IEx] Add `import_file/1` helper

* Backwards incompatible changes
  * [Enum] `Enum.Iterator` was replaced by the more composable and functional `Enumerable` protocol which supports reductions
  * [File] `File.iterator/1` and `File.biniterator/1` have been removed in favor of the safe `File.iterator!/1` and `File.biniterator!/1` ones
  * [Kernel] Erlang R15 is no longer supported
  * [Kernel] Elixir modules are now represented as `Elixir.ModuleName` (using `.` instead of `-` as separator)

# v0.8.3 (2013-05-22)

* Enhancements
  * [CLI] Flags `-p` and `-pr` fails if pattern match no files
  * [CLI] Support `--hidden` and `--cookie` flags for distributed Erlang
  * [Enum] Add `Enum.to_list/1`, `Enum.member?/2`, `Enum.uniq/2`, `Enum.max/1`, `Enum.max/2`, `Enum.min/1` and `Enum.min/2`
  * [ExUnit] Add `ExUnit.CaptureIO` for IO capturing during tests
  * [ExUnit] Consider load time on ExUnit time reports
  * [IEx] Support `ls` with colored output
  * [IEx] Add `#iex:break` to break incomplete expressions
  * [Kernel] Add `Enum.at`, `Enum.fetch` and `Enum.fetch!`
  * [Kernel] Add `String.to_integer` and `String.to_float`
  * [Kernel] Add `Dict.take`, `Dict.drop`, `Dict.split`, `Dict.pop` and `Dict.fetch!`
  * [Kernel] Many optimizations for code compilation
  * [Kernel] `in` can be used with right side expression outside guards
  * [Kernel] Add `Node.get_cookie/0` and `Node.set_cookie/2`
  * [Kernel] Add `__DIR__`
  * [Kernel] Expand macros and attributes on quote, import, alias and require
  * [Kernel] Improve warnings related to default arguments
  * [Keyword] Add `Keyword.delete_first/2`
  * [Mix] Add `local.rebar` to download a local copy of rebar, and change `deps.compile` to use it if needed
  * [Mix] Support umbrella applications
  * [Mix] Load beam files available at `MIX_PATH` on CLI usage
  * [String] Add `String.valid?` and `String.valid_character?`

* Bug fixes
  * [ExUnit] Handle exit messages from in ExUnit
  * [ExUnit] Failures on ExUnit's setup_all now invalidates all tests
  * [Kernel] Ensure we don't splice keyword args unecessarily
  * [Kernel] Private functions used by private macros no longer emit an unused warning
  * [Kernel] Ensure Elixir won't trip on empty receive blocks
  * [Kernel] `String.slice` now returns an empty string when out of range by 1
  * [Mix] Generate manifest files after compilation to avoid depending on directory timestamps and to remove unused .beam files
  * [Path] `Path.expand/2` now correctly expands `~` in the second argument
  * [Regex] Fix badmatch with `Regex.captures(%r/(.)/g, "cat")`
  * [URI] Downcase host and scheme and URIs

* Deprecations
  * [Code] `Code.eval` is deprecated in favor of `Code.eval_string`
  * [Exception] `Exception.format_entry` is deprecated in favor of `Exception.format_stacktrace_entry`
  * [ExUnit] `assert left inlist right` is deprecated in favor of `assert left in right`
  * [IO] `IO.getb` is deprecated in favor of `IO.getn`
  * [List] `List.member?/2` is deprecated in favor of `Enum.member?/2`
  * [Kernel] `var_context` in quote was deprecated in favor of `context`
  * [Kernel] `Enum.at!` and `Dict.get!` is deprecated in favor of `Enum.fetch!` and `Dict.fetch!`

* Backwards incompatible changes
  * [Dict] `List.Dict` was moved to `ListDict`
  * [IO] `IO.gets`, `IO.getn` and friends now return binaries when reading from stdio
  * [Kernel] Precedence of `|>` has changed to lower to support constructs like `1..5 |> Enum.to_list`
  * [Mix] `mix escriptize` now receives arguments as binaries

# v0.8.2 (2013-04-20)

* Enhancements
  * [ExUnit] Use ANSI escape codes in CLI output
  * [ExUnit] Include suite run time on CLI results
  * [ExUnit] Add support to doctests, allowing test cases to be generated from code samples
  * [File] Add `File.ls` and `File.ls!`
  * [IEx] Support `pwd` and `cd` helpers
  * [Kernel] Better error reporting for invalid bitstring generators
  * [Kernel] Improve meta-programming by allowing `unquote` on `def/2`, `defp/2`, `defmacro/2` and `defmacrop/2`
  * [Kernel] Add support to R16B new functions: `insert_elem/3` and `delete_elem/2`
  * [Kernel] Import conflicts are now lazily handled. If two modules import the same functions, it will fail only if the function is invoked
  * [Mix] Support `--cover` on mix test and `test_coverage` on Mixfiles
  * [Record] Each record now provides `Record.options` with the options supported by its `new` and `update` functions

* Bug fixes
  * [Binary] inspect no longer escapes standalone hash `#`
  * [IEx] The `h` helper can now retrieve docs for special forms
  * [Kernel] Record optimizations were not being triggered in functions inside the record module
  * [Kernel] Aliases defined inside macros should be carried over
  * [Kernel] Fix a bug where nested records could not use the Record[] syntax
  * [Path] Fix a bug on `Path.expand` when expanding paths starting with `~`

* Deprecations
  * [Kernel] `setelem/3` is deprecated in favor of `set_elem/3`
  * [Kernel] `function(:is_atom, 1)` is deprecated in favor of `function(is_atom/1)`

* Backwards incompatible changes
  * [Kernel] `unquote` now only applies to the closest quote. If your code contains a quote that contains another quote that calls unquote, it will no longer work. Use `Macro.escape` instead and pass your quoted contents up in steps, for example:

            quote do
              quote do: unquote(x)
            end

      should become:

            quote do
              unquote(Macro.escape(x))
            end

# v0.8.1 (2013-02-17)

* Enhancements
  * [ExUnit] Tests can now receive metadata set on setup/teardown callbacks
  * [ExUnit] Add support to ExUnit.CaseTemplate to share callbacks in between test cases
  * [IO] Add `IO.ANSI` to make it easy to write ANSI escape codes
  * [Kernel] Better support for Unicode lists
  * [Kernel] Reduce variables footprint in `case`/`receive` clauses
  * [Kernel] Disable native compilation when on_load attributes is present to work around an Erlang bug
  * [Macro] `Macro.expand` also considers macros from the current `__ENV__` module
  * [Mix] Improve support for compilation of `.erl` files
  * [Mix] Add support for compilation of `.yrl` and `.xrl` files
  * [OptionParser] Switches are now overridden by default but can be kept in order if chosen
  * [Typespec] Better error reporting for invalid typespecs

* Bug fixes
  * [Mix] Allow Mix projects to be generated with just one letter

* Backwards incompatible changes
  * [Kernel] `before_compile` and `after_compile` callbacks now receive the environment as first argument instead of the module

* Deprecations
  * [ExUnit] Explicitly defined test/setup/teardown functions are deprecated
  * [Kernel] Tidy up and clean `quote` API
  * [Kernel] Old `:local.(args)` syntax is deprecated
  * [Process] `Process.self` is deprecated in favor `Kernel.self`

# v0.8.0 (2013-01-28)

* Enhancements
  * [Binary] Support `<< "string" :: utf8 >>` as in Erlang
  * [Binary] Support `\a` escape character in binaries
  * [Binary] Support syntax shortcut for specifying size in bit syntax
  * [CLI] Support `--app` option to start an application and its dependencies
  * [Dict] Support `put_new` in `Dict` and `Keyword`
  * [Dict] Add `ListDict` and a faster `HashDict` implementation
  * [ExUnit] ExUnit now supports multiple runs in the same process
  * [ExUnit] Failures in ExUnit now shows a tailored stacktrace
  * [ExUnit] Introduce `ExUnit.ExpectationError` to provide better error messages
  * [Kernel] Introduce `Application.Behaviour` to define application module callbacks
  * [Kernel] Introduce `Supervisor.Behaviour` to define supervisors callbacks
  * [Kernel] More optimizations were added to Record handling
  * [Kernel] `?\x` and `?\` are now supported ways to retrieve a codepoint
  * [Kernel] Octal numbers can now be defined as `0777`
  * [Kernel] Improve macros hygiene regarding variables, aliases and imports
  * [Mix] Mix now starts the current application before run, iex, test and friends
  * [Mix] Mix now provides basic support for compiling `.erl` files
  * [Mix] `mix escriptize` only generates escript if necessary and accept `--force` and `--no-compile` as options
  * [Path] Introduce `Path` module to hold filesystem paths related functions
  * [String] Add `String.capitalize` and `String.slice`
  * [System] Add `System.tmp_dir`, `System.cwd` and `System.user_home`

* Bug fixes
  * [Kernel] `import` with `only` accepts functions starting with underscore
  * [String] `String.first` and `String.last` return nil for empty binaries
  * [String] `String.rstrip` and `String.lstrip` now verify if argument is a binary
  * [Typespec] Support `...` inside typespec's lists

* Backwards incompatible changes
  * [Kernel] The AST now allows metadata to be attached to each node. This means the second item in the AST is no longer an integer (representing the line), but a keywords list. Code that relies on the line information from AST or that manually generate AST nodes need to be properly updated

* Deprecations
  * [Dict] Deprecate `Binary.Dict` and `OrdDict` in favor of `HashDict` and `ListDict`
  * [File] Deprecate path related functions in favor of the module `Path`
  * [Kernel] The `/>` operator has been deprecated in favor of `|>`
  * [Mix] `Mix.Project.sources` is deprecated in favor of `Mix.Project.config_files`
  * [Mix] `mix iex` is no longer functional, please use `iex -S mix`
  * [OptionParser] `:flags` option was deprecated in favor of `:switches` to support many types

# v0.7.2 (2012-12-04)

* Enhancements
  * [CLI] `--debug-info` is now true by default
  * [ExUnit] Make ExUnit exit happen in two steps allowing developers to add custom `at_exit` hooks
  * [IEx] Many improvements to helpers functions `h/1`, `s/1` and others
  * [Kernel] Functions defined with `fn` can now handle many clauses
  * [Kernel] Raise an error if clauses with different arities are defined in the same function
  * [Kernel] `function` macro now accepts arguments in `M.f/a` and `f/a` formats
  * [Macro] Improvements to `Macro.to_binary`
  * [Mix] Mix now echoes the output as it comes when executing external commands such as git or rebar
  * [Mix] Mix now validates `application` callback's values
  * [Record] Record accessors are now optimized and can be up to 6x faster in some cases
  * [String] Support `\xXX` and `\x{HEX}` escape sequences in strings, char lists and regexes

* Bug fixes
  * [Bootstrap] Compiling Elixir source no longer fails if environment variables contain utf-8 entries
  * [IEx] IEx will now wait for all command line options to be processed before starting
  * [Kernel] Ensure proper stacktraces when showing deprecations

* Deprecations
  * [Enum] `Enum.qsort` is deprecated in favor of `Enum.sort`
  * [List] `List.sort` and `List.uniq` have been deprecated in favor of their `Enum` counterparts
  * [Record] Default-based generated functions are deprecated
  * [Typespec] Enhancements and deprecations to the `@spec/@callback` and the fun type syntax

# v0.7.1 (2012-11-18)

* Enhancements
  * [IEx] Only show documented functions and also show docs for default generated functions
  * [IO] Add `IO.binread`, `IO.binwrite` and `IO.binreadline` to handle raw binary file operations
  * [ExUnit] Add support for user configuration at `HOME/.ex_unit.exs`
  * [ExUnit] Add support for custom formatters via a well-defined behaviour
  * [Kernel] Add support for `defrecordp`
  * [Kernel] Improved dialyzer support
  * [Kernel] Improved error messages when creating functions with aliases names
  * [Mix] Improve SCM behaviour to allow more robust integration
  * [Mix] Changing deps information on `mix.exs` forces users to fetch new dependencies
  * [Mix] Support (parallel) requires on mix run
  * [Mix] Support `-q` when running tests to compile only changed files
  * [String] Support `String.downcase` and `String.upcase` according to Unicode 6.2.0
  * [String] Add support for graphemes in `String.length`, `String.at` and others
  * [Typespec] Support `@opaque` as attribute
  * [Typespec] Define a default type `t` for protocols and records
  * [Typespec] Add support for the access protocol in typespecs

* Bug fixes
  * [Kernel] Fix an issue where variables inside clauses remained unassigned
  * [Kernel] Ensure `defoverridable` functions can be referred in many clauses
  * [Kernel] Allow keywords as function names when following a dot (useful when integrating with erlang libraries)
  * [File] File is opened by default on binary mode instead of utf-8

* Deprecations
  * [Behaviour] `defcallback/1` is deprecated in favor of `defcallback/2` which matches erlang `@callbacks`
  * [Enum] `Enum.times` is deprecated in favor of using ranges
  * [System] `halt` moved to `System` module

# v0.7.0 (2012-10-20)

* Enhancements
  * [Behaviour] Add Behaviour with a simple callback DSL to define callbacks
  * [Binary] Add a Dict binary that converts its keys to binaries on insertion
  * [Binary] Optimize `Binary.Inspect` and improve inspect for floats
  * [CLI] Support `--detached` option
  * [Code] `Code.string_to_ast` supports `:existing_atoms_only` as an option in order to guarantee no new atoms is generated when parsing the code
  * [EEx] Support `<%%` and `<%#` tags
  * [ExUnit] Support `after_spawn` callbacks which are invoked after each process is spawned
  * [ExUnit] Support context data in `setup_all`, `setup`, `teardown` and `teardown_all` callbacks
  * [IEx] Support `after_spawn` callbacks which are invoked after each process is spawned
  * [Kernel] Better error messages when invalid options are given to `import`, `alias` or `require`
  * [Kernel] Allow partial application on literals, for example: `{ &1, &2 }` to build tuples or `[&1|&2]` to build cons cells
  * [Kernel] Added `integer_to_binary` and `binary_to_integer`
  * [Kernel] Added `float_to_binary` and `binary_to_float`
  * [Kernel] Many improvements to `unquote` and `unquote_splicing`. For example, `unquote(foo).unquote(bar)(args)` is supported and no longer need to be written via `apply`
  * [Keyword] Keyword list is no longer ordered according to Erlang terms but the order in which they are specified
  * [List] Add `List.keyreplace` and `List.keystore`
  * [Macro]  Support `Macro.safe_term` which returns `:ok` if an expression does not execute code and is made only of raw data types
  * [Mix] Add support for environments - the current environment can be set via `MIX_ENV`
  * [Mix] Add support for handling and fetching dependencies' dependencies
  * [Module] Support module creation via `Module.create`
  * [Range] Support decreasing ranges
  * [Record] Improvements to the Record API, added `Record.defmacros`
  * [Regex] Add `:return` option to `Regex.run` and `Regex.scan`
  * [String] Add a String module responsible for handling UTf-8 binaries

* Bug fixes
  * [File] `File.cp` and `File.cp_r` now preserves the file's mode
  * [IEx] Fix a bug where printing to `:stdio` on `IEx` was causing it to hang
  * [Macro] Fix a bug where quoted expressions were not behaving the same as their non-quoted counterparts
  * [Mix] `mix deps.get [DEPS]` now only gets the specified dependencies
  * [Mix] Mix now exits with status 1 in case of failures
  * [Protocol] Avoid false positives on protocol dispatch (a bug caused the dispatch to be triggered to an invalid protocol)

* Backwards incompatible changes
  * [ExUnit] `setup` and `teardown` callbacks now receives the test name as second argument
  * [Kernel] Raw function definition with `def/4`, `defp/4`, `defmacro/4`, `defmacrop/4` now evaluates all arguments. The previous behaviour was accidental and did not properly evaluate all arguments
  * [Kernel] Change tuple-related (`elem` and `setelem`), Enum functions (`find_index`, `nth!` and `times`) and List functions (List.key*) to zero-index

* Deprecations
  * [Code] `Code.require_file` and `Code.load_file` now expect the full name as argument
  * [Enum] `List.reverse/1` and `List.zip/2` were moved to `Enum`
  * [GenServer] Rename `GenServer.Behavior` to `GenServer.Behaviour`
  * [Kernel] Bitstring syntax now uses `::` instead of `|`
  * [Kernel] `Erlang.` syntax is deprecated in favor of simply using atoms
  * [Module] `Module.read_attribute` and `Module.add_attribute` deprecated in favor of `Module.get_attribute` and `Module.put_attribute` which mimics Dict API

# v0.6.0 (2012-08-01)

* Backwards incompatible changes
  * [Kernel] Compile files now follow `Elixir-ModuleName` convention to solve issues with Erlang embedded mode. This removes the `__MAIN__` pseudo-variable as modules are now located inside `Elixir` namespace
  * [Kernel] `__using__` callback triggered by `use` now receives just one argument. Caller information can be accessed via macros using `__CALLER__`
  * [Kernel] Comprehensions syntax changed to be more compatible with Erlang behaviour
  * [Kernel] loop and recur are removed in favor of recursion with named functions
  * [Module] Removed data functions in favor of unifying the attributes API

* Deprecations
  * [Access] The semantics of the access protocol were reduced from a broad query API to simple data structure key-based access
  * [ExUnit] Some assertions are deprecated in favor of simply using `assert()`
  * [File] `File.read_info` is deprecated in favor of `File.stat`
  * [IO] `IO.print` is deprecated in favor of `IO.write`
  * [Kernel] Deprecate `__LINE__` and `__FUNCTION__` in favor of `__ENV__.line` and `__ENV__.function`
  * [Kernel] Deprecate `in_guard` in favor of `__CALLER__.in_guard?`
  * [Kernel] `refer` is deprecated in favor of `alias`
  * [Module] `Module.add_compile_callback(module, target, callback)` is deprecated in favor of `Module.put_attribute(module, :before_compile, { target, callback })`
  * [Module] `Module.function_defined?` is deprecated in favor of `Module.defines?`
  * [Module] `Module.defined_functions` is deprecated in favor of `Module.definitions_in`

* Enhancements
  * [Enum] Enhance Enum protocol to support `Enum.count`
  * [Enum] Optimize functions when a list is given as collection
  * [Enum] Add `find_index`, `nth!` and others
  * [ExUnit] Support setup and teardown callbacks
  * [IEx] IEx now provides autocomplete if the OS supports tty
  * [IEx] IEx now supports remsh
  * [IEx] Elixir now defaults to compile with documentation and `d` can be used in IEx to print modules and functions documentation
  * [IEx] Functions `c` and `m` are available in IEx to compile and print available module information. Functions `h` and `v` are available to show history and print previous commands values
  * [IO/File] Many improvements to `File` and `IO` modules
  * [Kernel] Operator `!` is now allowed in guard clauses
  * [Kernel] Introduce operator `=~` for regular expression matches
  * [Kernel] Compiled docs now include the function signature
  * [Kernel] `defmodule` do not start a new variable scope, this improves meta-programming capabilities
  * [Kernel] quote special form now supports line and unquote as options
  * [Kernel] Document the macro `@` and allow attributes to be read inside functions
  * [Kernel] Add support to the `%R` sigil. The same as `%r`, but without interpolation or escaping. Both implementations were also optimized to generate the regex at compilation time
  * [Kernel] Add `__ENV__` which returns a `Macro.Env` record with information about the compilation environment
  * [Kernel] Add `__CALLER__` inside macros which returns a `Macro.Env` record with information about the calling site
  * [Macro] Add `Macro.expand`, useful for debugging what a macro expands to
  * [Mix] First Mix public release
  * [Module] Add support to `@before_compile` and `@after_compile` callbacks. The first receives the module name while the latter receives the module name and its object code
  * [OptionParser] Make OptionParser public, add support to flags and improved switch parsing
  * [Range] Add a Range module with support to `in` operator (`x in 1..3`) and iterators
  * [Record] Allow `Record[_: value]` to set a default value to all records fields, as in Erlang
  * [Record] Records now provide a `to_keywords` function
  * [Regex] Back references are now properly supported
  * [System] Add `System.find_executable`

# v0.5.0 (2012-05-24)

* First official release
