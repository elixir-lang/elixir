* enhancements
  * [Behaviour] Add Behaviour with a simple callback DSL to define callbacks
  * [Binary] Add a Dict binary that converts its keys to binaries on insertion
  * [ExUnit] Supports `after_spawn` callbacks which are invoked after each process is spawned
  * [IEx] Supports `after_spawn` callbacks which are invoked after each process is spawned
  * [Kernel] Better error messages when invalid options are given to `import`, `alias` or `require`
  * [List] Add support to keyreplace and keystore
  * [Mix] Mix now support environments - the current environment can be set via MIX_ENV
  * [Record] Improvements to the Record API, added `Record.defmacros`
  * [Regex] Added return option to `Regex.run` and `Regex.scan`
  * [String] Add a String module responsible for handling UTf-8 binaries

* bug fix
  * [IEx] Fixed a bug where printing to stdio on IEx was causing it to hang
  * [Macro] Fixed a bug where quoted expressions were not behaving the same as their non-quoted counterparts
  * [Mix] `mix deps.get [DEPS]` now only gets the specified dependencies
  * [Mix] Mix now exits with status 1 in case of failures

* backwards incompatible changes
  * [Kernel] Raw function definition with def/4, defp/4, defmacro/4, defmacrop/4 now evaluates all arguments. The previous behaviour was accidental and did not properly evaluate all arguments
  * [Kernel] Changed tuple-related (`elem` and `setelem`), Enum functions (`find_index` and `nth!`) and List functions (List.key*) to zero-index

* deprecations
  * [Code] `Code.require_file` and `Code.load_file` now expect the full name as argument
  * [GenServer] Rename `GenServer.Behavior` to `GenServer.Behaviour`
  * [Kernel] Bitstring syntax now uses `::` instead of `|`

# v0.6.0 (2012-08-01)

* incompatible changes
  * [Kernel] Compiled files now follow `Elixir-ModuleName` convention to solve issues with Erlang embedded mode. This removes the `__MAIN__` pseudo-variable as modules are now located inside `Elixir` namespace
  * [Kernel] `__using__` callback triggered by `use` now receives just one argument. Caller information can be accessed via macros using `__CALLER__`
  * [Kernel] Comprehensions syntax changed to be more compatible with Erlang behavior
  * [Kernel] loop and recur were removed in favor of recursion with named functions
  * [Module] Removed data functions in favor of unifying the attributes API

* deprecations
  * [Access] The semantics of the access protocol were reduced from a broad query API to simple data structure key-based access
  * [ExUnit] Some assertions were deprecated in favor of simply using `assert()` 
  * [File] `File.read_info` was deprecated in favor of `File.stat`
  * [IO] `IO.print` was deprecated in favor of `IO.write`
  * [Kernel] Deprecated `__LINE__` and `__FUNCTION__` in favor of `__ENV__.line` and `__ENV__.function`
  * [Kernel] Deprecated `in_guard` in favor of `__CALLER__.in_guard?`
  * [Kernel] `refer` is deprecated in favor of `alias`
  * [Module] `Module.add_compile_callback(module, target, callback)` was deprecated in favor of `Module.add_attribute(module, :before_compile, { target, callback })`
  * [Module] `Module.function_defined?` was deprecated in favor of `Module.defines?`
  * [Module] `Module.defined_functions` was deprecated in favor of `Module.definitions_in`

* enhancements
  * [Enum] Enhanced Enum protocol to support `Enum.count`
  * [Enum] Optimized functions when a list is given as collection
  * [Enum] Added `find_index`, `nth!` and others
  * [ExUnit] Support setup and teardown callbacks
  * [IEx] IEx now provides autocomplete if the OS supports tty
  * [IEx] IEx now supports remsh
  * [IEx] Elixir now defaults to compile with documentation and `d` can be used in IEx to print modules and functions documentation
  * [IEx] Functions `c` and `m` are available in IEx to compile and print available module information. Functions `h` and `v` are available to show history and print previous commands values
  * [IO/File] Many improvements to `File` and `IO` modules
  * [Kernel] Operator `!` is now allowed in guard clauses
  * [Kernel] Introduced operator `=~` for regular expression matches
  * [Kernel] Compiled docs now include the function signature
  * [Kernel] `defmodule` do not start a new variable scope, this improves meta-programming capabilities
  * [Kernel] quote special form now supports line and unquote as options
  * [Kernel] Document the macro `@` and allow attributes to be read inside functions
  * [Kernel] Added support to the `%R` sigil. The same as `%r`, but without interpolation or escaping. Both implementations were also optimized to generate the regex at compilation time
  * [Kernel] Added `__ENV__` which returns a `Macro.Env` record with information about the compilation environment
  * [Kernel] Added `__CALLER__` inside macros which returns a `Macro.Env` record with information about the calling site
  * [Macro] Added `Macro.expand`, useful for debugging what a macro expands to
  * [Mix] First Mix public release
  * [Module] Added support to `@before_compile` and `@after_compile` callbacks. The first receives the module name while the latter receives the module name and its object code
  * [OptionParser] Make OptionParser public, add support to flags and improved switch parsing
  * [Range] Added a Range module with support to `in` operator (`x in 1..3`) and iterators
  * [Record] Allow `Record[_: value]` to set a default value to all records fields, as in Erlang
  * [Record] Records now provide a `to_keywords` function
  * [Regex] Back references are now properly supported
  * [System] Added `System.find_executable`

# v0.5.0 (2012-05-24)

* First official release