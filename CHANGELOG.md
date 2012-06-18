* backwards incompatible changes
  * [Builtin] `__using__` callback triggered by `use` now receives just one argument. Caller information can be accessed in the `__CALLER__`;
  * [Module] Removed data functions in favor of unifying the attributes API;
  * [Kernel] Comprehensions syntax changed to be more compatible with Erlang behavior;
  * [Kernel] Compiled files now follow "__MAIN__-ModuleName" convention to solve issues with Erlang embedded mode;

* deprecations
  * [Module] `Module.add_compile_callback(module, target, callback)` was deprecated in favor of the simpler `Module.add_attribute(module, :before_compile, { target, callback })` API;
  * [File] `File.read_info` was deprecated in favor of `File.stat`;
  * [IO] `IO.print` was deprecated in favor of `IO.write`;
  * [Kernel] Deprecated `__LINE__` and `__FUNCTION__` in favor of `__ENV__.line` and `__ENV__.function`;
  * [Kernel] Deprecated `in_guard` in favor of `__CALLER__.in_guard?`;
  * [Kernel] `refer` is deprecated in favor of `alias`;

* enhancements
  * [Range] Added a Range module with support to `in` operator (`x in 1..3`) and iterators;
  * [Enum] Enhanced Enum protocol to support `Enum.count`;
  * [Module] Added support to `@before_compile` and `@after_compile` callbacks. The first receives the module name while the latter receives the module name and its object code;
  * [Kernel] quote special form now supports line and unquote as options;
  * [Record] Allow `Record[_: value]` to set a default value to all records fields, as in Erlang;
  * [IEx] Functions `c` and `m` are available in IEx to compile and print available module information;
  * [Enum] Optimized functions when a list is given as collection;
  * [System] Added `System.find_executable`
  * [Builtin] Document the macro `@` and allow attributes to be read inside functions;
  * [IO/File] Many improvements to `File` and `IO` modules;
  * [Macro] Added `Macro.expand`, useful for debugging what a macro expands to;
  * [Enum] Added `find_index`;
  * [Record] Records now provide a `to_keywords` function;
  * [Builtin] Added support to the `%R` sigil. The same as `%r`, but without interpolation or escaping. Both implementations were also optimized to generate the regex at compilation time;
  * [Kernel] Added `__ENV__` which returns a `Macro.Env` record with information about the compilation environment;
  * [Kernel] Added `__CALLER__` inside macros which returns a `Macro.Env` record with information about the calling site;

# v0.5.0 (2012-05-24)

* First official release