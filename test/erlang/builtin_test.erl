-module(builtin_test).
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

dict_in_method_calls_test() ->
  F = fun() ->
    elixir:eval("module Bar\nmixin self\ndef foo(x);x;end\ndef bar(x,y); [x,y]; end\nend"),
    {Dict1,[]} = elixir:eval("Bar.foo('a: 1)"),
    {ok, 1} = dict:find(a, Dict1),
    {Dict2,[]} = elixir:eval("Bar.foo 'a: 1"),
    {ok, 1} = dict:find(a, Dict2),
    {[1, Dict3],[]} = elixir:eval("Bar.bar(1, 'a: 1)"),
    {ok, 1} = dict:find(a, Dict3),
    {[1, Dict4],[]} = elixir:eval("Bar.bar 1, 'a: 1"),
    {ok, 1} = dict:find(a, Dict4),
    {[Dict5, Dict6],[]} = elixir:eval("Bar.bar({'b: 2}, 'a: 1)"),
    {ok, 2} = dict:find(b, Dict5),
    {ok, 1} = dict:find(a, Dict6),
    {[Dict7, Dict8],[]} = elixir:eval("Bar.bar {'b: 2}, 'a: 1"),
    {ok, 2} = dict:find(b, Dict7),
    {ok, 1} = dict:find(a, Dict8)
  end,
  test_helper:run_and_remove(F, ['Bar']).
