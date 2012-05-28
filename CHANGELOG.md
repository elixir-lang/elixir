* deprecations
  * [Kernel] Deprecated `__LINE__` and `__FUNCTION__` in favor of `__ENV__.line` and `__ENV__.function`;
  * [Kernel] Deprecated `in_guard` in favor of `__CALLER__.in_guard?`;

* enhancements
  * [Builtin] Added support to the `%R` sigil. The same as `%r`, but without interpolation or escaping;
  * [Kernel] Added `__ENV__` which returns a `Macro.Env` record with information about the compilation environment;
  * [Kernel] Added `__CALLER__` inside macros which returns a `Macro.Env` record with information about the calling site;

# v0.5.0 (2012-05-24)

* First official release