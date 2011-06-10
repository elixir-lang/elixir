## Current master (v0.3.0.dev)

* improvements
  * ExUnit now run test cases in parallel
  * Added anonymous methods: def(x,y) x + y; end
  * Added Method and UnboundMethod modules
  * Added `using` as temporary mixins
  * Added `respond_to?(method, arity)`
  * Nested brackets are now supported `nested_array[1][2]`
  * Brackets method in array was optimized and also accepts out of bound indexes (returning nil in such cases)
  * Brackets method in tuples accepts out of bound indexes (by github.com/marcinbunsch)
  * Local calls are restricted only to private methods
  * Added `Timer` with the method `ms` used for benchmarking

* deprecations
  * Use {} instead of {:} as an empty dict. An empty tuple can be retrieved with Tuple.empty
  * `__added_as_mixin__` is now `__mixed_in__`
  * object and protos were removed in favor of module binding

## v0.2.0 (2011-04-25)

* improvements
  * Added File.read, File.read_info and others
  * Added basic support to ets tables (by github.com/kondratovich)
  * Added nil as value (which also evaluates to false)
  * Added the ability to use _ in number for readability: 123\_523\_542 (by github.com/kondratovich)
  * Added the ability to add default arguments to methods
  * Added @() as short syntax to update instance variables
  * Elixir now boots as an Erlang application under a supervision tree (by github.com/maxlapshin)
  * ExUnit now prints "." or "F" for each test executed
  * Added an interface to Erlang's GenTCP (by github.com/justinbaker)
  * Added the ability to import records from Erlang
  * Added more methods to OrderedDict, List and File in STDLIB
  * Improved performance on method dispatch about 8 times
  * Number operators are 'atom/value are allowed in method signatures
  * Allow variables to be assigned more than once
  * Basic support for guards
  * Added Date to STDLIB (by github.com/justinbaker)
  * Added Set to STDLIB (by github.com/lsylvester)
  * Added DateTime to STDLIB
  * Allow the cache directive to specify dependencies
  * Added elixirc as an explicit compiler step and autoload
  * Functions can be called as fun.call(arg1, arg2), fun.apply([arg1,arg2]), fun[arg1, arg2] or fun.(arg1, arg2)

* bug fix
  * Allow Elixir keywords, but that are not Erlang keywords, on Erlang method invocations
  * Code formatter now properly handles empty arrays

* deprecations
  * protected methods were removed for performance reasons, may eventually be brought back
  * Removed constructor in favor of initialize
  * Removed require in favor of autoload
  * Calling functions as fun() is no longer supported

## v0.1.0 (2011-03-12)

* Initial release