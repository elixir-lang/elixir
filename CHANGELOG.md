## Current master (v0.2.0.dev)

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

* bug fix
  * Allow Elixir keywords, but that are not Erlang keywords, on Erlang method invocations
  * Code formatter now properly handles empty arrays

* deprecations
  * protected methods were removed for performance reasons, may eventually be brought back
  * remove constructor in favor of initialize

## v0.1.0 (2011-03-12)

* Initial release