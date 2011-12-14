-module(namespace_test).
-include_lib("eunit/include/eunit.hrl").

namespace_definition_test() ->
  elixir:eval("ns Foo::Bar::Baz").