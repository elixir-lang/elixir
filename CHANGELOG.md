* enhancements
  * [CLI] Flags `-p` and `-pr` fails if pattern match no files
  * [CLI] Support `--hidden` and `--cookie` flags for distributed Erlang
  * [Enum] Add `Enum.to_list/1`, `Enum.equal?/2`, `Enum.equal?/3`, `Enum.member?/2`, `Enum.uniq/2`, `Enum.max/1`, `Enum.max/2`, `Enum.min/1` and `Enum.min/2`
  * [ExUnit] Add `ExUnit.CaptureIO` for IO capturing during tests
  * [IEx] Support `ls` with colored output
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
  * [String] Add `String.valid?` and `String.valid_character?`

* bug fix
  * [ExUnit] Handle exit messages from in ExUnit
  * [ExUnit] Failures on ExUnit's setup_all now invalidates all tests
  * [Kernel] Ensure we don't splice keyword args unecessarily
  * [Kernel] Private functions used by private macros no longer emit an unused warning 
  * [Kernel] Ensure Elixir won't trip on empty receive blocks
  * [Kernel] `String.slice` now returns an empty string when out of range by 1
  * [Mix] Generate manifest files after compilation to avoid depending on directory timestamps and to remove unused .beam files
  * [Regex]  Fix badmatch with Regex.captures(%r/(.)/g, "cat")
  * [URI] Downcase host and scheme and URIs

* deprecations
  * [ExUnit] `assert left inlist right` is deprecated in favor of `assert left in right`
  * [Kernel] `List.member?/2` is deprecated in favor of `Enum.member?/2`
  * [Kernel] `var_context` in quote was deprecated in favor of `context`
  * [Kernel] `Enum.at!` and `Dict.get!` is deprecated in favor of `Enum.fetch!` and `Dict.fetch!`

* backwards incompatible changes
  * [Kernel] `IO.gets`, `IO.getb` and friends now return binaries when reading from stdio
  * [Kernel] `List.Dict` was moved to `ListDict`
  * [Kernel] Precedence of `|>` has changed to lower to support constructs like `1..5 |> Enum.to_list`
  * [Mix] `mix escriptize` now receives arguments as binaries

# v0.8.2 (2013-04-20)

* enhancements
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

* bug fix
  * [Binary] inspect no longer escapes standalone hash `#`
  * [IEx] The `h` helper can now retrieve docs for special forms
  * [Kernel] Record optimizations were not being triggered in functions inside the record module
  * [Kernel] Aliases defined inside macros should be carried over
  * [Kernel] Fix a bug where nested records could not use the Record[] syntax
  * [Path] Fix a bug on `Path.expand` when expanding paths starting with `~`

* deprecations
  * [Kernel] `setelem/3` is deprecated in favor of `set_elem/3`
  * [Kernel] `function(:is_atom, 1)` is deprecated in favor of `function(is_atom/1)`

* backwards incompatible changes
  * [Kernel] `unquote` now only applies to the closest quote. If your code contains a quote that contains another quote that calls unquote, it will no longer work. Use `Macro.escape` instead and pass your quoted contents up in steps, for example:

            quote do
              quote do: unquote(x)
            end

      should become:

            quote do
              unquote(Macro.escape(x))
            end

# v0.8.1 (2013-02-17)

* enhancements
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

* bug fix
  * [Mix] Allow Mix projects to be generated with just one letter

* backwards incompatible changes
  * [Kernel] `before_compile` and `after_compile` callbacks now receive the environment as first argument instead of the module

* deprecations
  * [ExUnit] Explicitly defined test/setup/teardown functions are deprecated
  * [Kernel] Tidy up and clean `quote` API
  * [Kernel] Old `:local.(args)` syntax is deprecated
  * [Process] `Process.self` is deprecated in favor `Kernel.self`

# v0.8.0 (2013-01-28)

* enhancements
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

* bug fix
  * [Kernel] `import` with `only` accepts functions starting with underscore
  * [String] `String.first` and `String.last` return nil for empty binaries
  * [String] `String.rstrip` and `String.lstrip` now verify if argument is a binary
  * [Typespec] Support `...` inside typespec's lists

* backwards incompatible changes
  * [Kernel] The AST now allows metadata to be attached to each node. This means the second item in the AST is no longer an integer (representing the line), but a keywords list. Code that relies on the line information from AST or that manually generate AST nodes need to be properly updated

* deprecations
  * [Dict] Deprecate `Binary.Dict` and `OrdDict` in favor of `HashDict` and `ListDict`
  * [File] Deprecate path related functions in favor of the module `Path`
  * [Kernel] The `/>` operator has been deprecated in favor of `|>`
  * [Mix] `Mix.Project.sources` is deprecated in favor of `Mix.Project.config_files`
  * [Mix] `mix iex` is no longer functional, please use `iex -S mix`
  * [OptionParser] `:flags` option was deprecated in favor of `:switches` to support many types

# v0.7.2 (2012-12-04)

* enhancements
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

* bug fix
  * [Bootstrap] Compiling Elixir source no longer fails if environment variables contain utf-8 entries
  * [IEx] IEx will now wait for all command line options to be processed before starting
  * [Kernel] Ensure proper stacktraces when showing deprecations

* deprecations
  * [Enum] `Enum.qsort` is deprecated in favor of `Enum.sort`
  * [List] `List.sort` and `List.uniq` have been deprecated in favor of their `Enum` counterparts
  * [Record] Default-based generated functions are deprecated
  * [Typespec] Enhancements and deprecations to the `@spec/@callback` and the fun type syntax

# v0.7.1 (2012-11-18)

* enhancements
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

* bug fix
  * [Kernel] Fix an issue where variables inside clauses remained unassigned
  * [Kernel] Ensure `defoverridable` functions can be referred in many clauses
  * [Kernel] Allow keywords as function names when following a dot (useful when integrating with erlang libraries)
  * [File] File is opened by default on binary mode instead of utf-8

* deprecations
  * [Behaviour] `defcallback/1` is deprecated in favor of `defcallback/2` which matches erlang `@callbacks`
  * [Enum] `Enum.times` is deprecated in favor of using ranges
  * [System] `halt` moved to `System` module

# v0.7.0 (2012-10-20)

* enhancements
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

* bug fix
  * [File] `File.cp` and `File.cp_r` now preserves the file's mode
  * [IEx] Fix a bug where printing to `:stdio` on `IEx` was causing it to hang
  * [Macro] Fix a bug where quoted expressions were not behaving the same as their non-quoted counterparts
  * [Mix] `mix deps.get [DEPS]` now only gets the specified dependencies
  * [Mix] Mix now exits with status 1 in case of failures
  * [Protocol] Avoid false positives on protocol dispatch (a bug caused the dispatch to be triggered to an invalid protocol)

* backwards incompatible changes
  * [ExUnit] `setup` and `teardown` callbacks now receives the test name as second argument
  * [Kernel] Raw function definition with `def/4`, `defp/4`, `defmacro/4`, `defmacrop/4` now evaluates all arguments. The previous behaviour was accidental and did not properly evaluate all arguments
  * [Kernel] Change tuple-related (`elem` and `setelem`), Enum functions (`find_index`, `nth!` and `times`) and List functions (List.key*) to zero-index

* deprecations
  * [Code] `Code.require_file` and `Code.load_file` now expect the full name as argument
  * [Enum] `List.reverse/1` and `List.zip/2` were moved to `Enum`
  * [GenServer] Rename `GenServer.Behavior` to `GenServer.Behaviour`
  * [Kernel] Bitstring syntax now uses `::` instead of `|`
  * [Kernel] `Erlang.` syntax is deprecated in favor of simply using atoms
  * [Module] `Module.read_attribute` and `Module.add_attribute` deprecated in favor of `Module.get_attribute` and `Module.put_attribute` which mimics Dict API

# v0.6.0 (2012-08-01)

* incompatible changes
  * [Kernel] Compile files now follow `Elixir-ModuleName` convention to solve issues with Erlang embedded mode. This removes the `__MAIN__` pseudo-variable as modules are now located inside `Elixir` namespace
  * [Kernel] `__using__` callback triggered by `use` now receives just one argument. Caller information can be accessed via macros using `__CALLER__`
  * [Kernel] Comprehensions syntax changed to be more compatible with Erlang behavior
  * [Kernel] loop and recur are removed in favor of recursion with named functions
  * [Module] Removed data functions in favor of unifying the attributes API

* deprecations
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

* enhancements
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
