-module(erlang_call_test).
-export([match/0]).
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, elixir_transform}).

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

match() -> abs.

erlang_atoms_test() ->
  {abs, []} = eval(":abs").

erlang_tuple_test() ->
  {[erlang,1,2], []} = eval("{ :erlang, 1, 2 }.tuple_to_list").

erlang_local_test() ->
  {1, []} = eval(":abs.(-1)").

erlang_call_test() ->
  {1, []}   = eval(":erlang.abs(-1)"),
  {_, []}   = eval(":dict.new"),
  {_, []}   = eval(":dict.new()"),
  {abs, []} = eval(":erlang_call_test.match()").