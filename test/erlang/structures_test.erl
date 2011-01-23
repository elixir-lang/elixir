-module(structures_test).
-include_lib("eunit/include/eunit.hrl").

simple_dict_test() ->
  {Dict, _} = elixir:eval("{ 'a: 1 }"),
  {ok, 1} = dict:find(a, Dict),
  error = dict:find(b, Dict).

empty_dict_test() ->
  {Dict, _} = elixir:eval("{:}"),
  error = dict:find(b, Dict).

dict_with_variables_test() ->
  {Dict, _} = elixir:eval("a = 'a\nb = 1\n{ a: b, 'b: 2}"),
  {ok, 1} = dict:find(a, Dict),
  {ok, 2} = dict:find(b, Dict),
  error = dict:find(c, Dict).

dict_with_assignment_test() ->
  {Dict, [{a, a}, {b, 1}]} = elixir:eval("{ (a = 'a): (b = 1) }"),
  {ok, 1} = dict:find(a, Dict),
  error = dict:find(b, Dict).
