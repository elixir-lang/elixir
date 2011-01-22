-module(erlang_call_test).
-include_lib("eunit/include/eunit.hrl").

erlang_call_test() ->
  {1, []} = elixir:eval("Erlang.abs(-1)"),
  {1, []} = elixir:eval("Erlang.erlang.abs(-1)").
