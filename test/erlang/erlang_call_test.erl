-module(erlang_call_test).
-include_lib("eunit/include/eunit.hrl").

erlang_call_test() ->
  {1, []} = elixir:eval("erl.abs(-1)"),
  {1, []} = elixir:eval("erl.erlang.abs(-1)").
