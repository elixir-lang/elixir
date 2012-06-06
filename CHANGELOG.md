* backwards incompatible changes
  * [Builtin] `__using__` callback triggered by `use` now receives just one argument. Caller information can be accessed in the `__CALLER__`;
  * [Module] Removed data functions in favor of unifying the attributes API;
  * [Kernel] Comprehensions syntax changed to be more compatible with Erlang behavior;

* deprecations
  * [File] `File.read_info` was deprecated in favor of `File.stat`;
  * [IO] `IO.print` was deprecated in favor of `IO.write`;
  * [Kernel] Deprecated `__LINE__` and `__FUNCTION__` in favor of `__ENV__.line` and `__ENV__.function`;
  * [Kernel] Deprecated `in_guard` in favor of `__CALLER__.in_guard?`;
  * [Kernel] `refer` is deprecated in favor of `alias`;

* enhancements
  * [Builtin] Document the macro `@` and allow attributes to be read inside functions;
  * [IO/File] Many improvements to File and IO modules;
  * [Macro] Added `Macro.expand`, useful for debugging what a macro expands to;
  * [Enum] Added `find_index`;
  * [Record] Records now provide a `to_keywords` function;
  * [Builtin] Added support to the `%R` sigil. The same as `%r`, but without interpolation or escaping;
  * [Kernel] Added `__ENV__` which returns a `Macro.Env` record with information about the compilation environment;
  * [Kernel] Added `__CALLER__` inside macros which returns a `Macro.Env` record with information about the calling site;

# v0.5.0 (2012-05-24)

* First official release