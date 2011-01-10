-module(arithmetic_test).
-include_lib("eunit/include/eunit.hrl").

equals_test() ->
    ?assert(elixir:parse("1+2")).
