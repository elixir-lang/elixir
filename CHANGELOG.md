* enhancements
  * [Kernel] Better error messages when invalid options are given to `import`, `alias` or `require`

# v0.6.0 (2012-08-01)

* incompatible changes
  * [Kernel] Compiled files now follow `Elixir-ModuleName` convention to solve issues with Erlang embedded mode. This removes the `__MAIN__` pseudo-variable as modules are now located inside `Elixir` namespace
  * [Kernel] `__using__` callback triggered by `use` now receives just one argument. Caller information can be accessed via macros using `__CALLER__`
  * [Module] Removed data functions in favor of unifying the attributes API
  * [Kernel] Comprehensions syntax changed to be more compatible with Erlang behavior
  * [Kernel] loop and recur were removed in favor of recursion with named functions

* deprecations
  * [Access] The semantics of the access protocol were reduced from a broad query API to simple data structure key-based access
  * [Module] `Module.add_compile_callback(module, target, callback)` was deprecated in favor of `Module.add_attribute(module, :before_compile, { target, callback })`
  * [Module] `Module.function_defined?` was deprecated in favor of `Module.defines?`
  * [Module] `Module.defined_functions` was deprecated in favor of `Module.definitions_in`
  * [File] `File.read_info` was deprecated in favor of `File.stat`
  * [IO] `IO.print` was deprecated in favor of `IO.write`
  * [Kernel] Deprecated `__LINE__` and `__FUNCTION__` in favor of `__ENV__.line` and `__ENV__.function`
  * [Kernel] Deprecated `in_guard` in favor of `__CALLER__.in_guard?`
  * [Kernel] `refer` is deprecated in favor of `alias`
  * [ExUnit] Some assertions were deprecated in favor of simply using `assert()` 

* enhancements
  * [OptionParser] Make OptionParser public, add support to flags and improved switch parsing
  * [Kernel] Operator `!` is now allowed in guard clauses
  * [IEx] IEx now provides autocomplete if the OS supports tty
  * [IEx] IEx now supports remsh
  * [Mix] First Mix public release
  * [Regex] Back references are now properly supported
  * [IEx] Elixir now defaults to compile with documentation and `d` can be used in IEx to print modules and functions documentation
  * [ExUnit] Support setup and teardown callbacks
  * [Kernel] Introduced operator `=~` for regular expression matches
  * [Kernel] Compiled docs now include the function signature
  * [Kernel] `defmodule` do not start a new variable scope, this improves meta-programming capabilities
  * [Range] Added a Range module with support to `in` operator (`x in 1..3`) and iterators
  * [Enum] Enhanced Enum protocol to support `Enum.count`
  * [Module] Added support to `@before_compile` and `@after_compile` callbacks. The first receives the module name while the latter receives the module name and its object code
  * [Kernel] quote special form now supports line and unquote as options
  * [Record] Allow `Record[_: value]` to set a default value to all records fields, as in Erlang
  * [IEx] Functions `c` and `m` are available in IEx to compile and print available module information. Functions `h` and `v` are available to show history and print previous commands values
  * [Enum] Optimized functions when a list is given as collection
  * [System] Added `System.find_executable`
  * [Kernel] Document the macro `@` and allow attributes to be read inside functions
  * [IO/File] Many improvements to `File` and `IO` modules
  * [Macro] Added `Macro.expand`, useful for debugging what a macro expands to
  * [Enum] Added `find_index`, `nth!` and others
  * [Record] Records now provide a `to_keywords` function
  * [Kernel] Added support to the `%R` sigil. The same as `%r`, but without interpolation or escaping. Both implementations were also optimized to generate the regex at compilation time
  * [Kernel] Added `__ENV__` which returns a `Macro.Env` record with information about the compilation environment
  * [Kernel] Added `__CALLER__` inside macros which returns a `Macro.Env` record with information about the calling site

# v0.5.0 (2012-05-24)

* First official release