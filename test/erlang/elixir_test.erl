% Holds all bootstraping assertions.
-module(elixir_test).
-include_lib("eunit/include/eunit.hrl").

object_bootstrap_mixins_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.mixins").

object_bootstrap_protos_test() ->
  {['Object::Methods'], []} = elixir:eval("Object.protos").