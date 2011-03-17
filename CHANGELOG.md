## Current master (v0.1.1.dev)

* improvements
  * Added nil as value (which also evaluates to false)
  * Added the ability to use _ in number for readability: 123\_523\_542 (by github.com/kondratovich)
  * Added the ability to add default arguments to methods
  * Added @() as short syntax to update instance variables
  * Elixir now boots as an Erlang application under a supervision tree (by github.com/maxlapshin)
  * ExUnit now prints "." or "F" for each test executed
  * Added an interface to Erlang's GenTCP (by github.com/justinbaker)
  * Added the ability to import records from Erlang
  * Added more methods to OrderedDict, List and File in STDLIB

* bug fix
  * Allow Elixir keywords, but that are not Erlang keywords, on Erlang method invocations
  * Code formatter now properly handles empty arrays

## v0.1.0 (2011-03-12)

* Initial release