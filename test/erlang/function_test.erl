-module(function_test).
-include_lib("eunit/include/eunit.hrl").

function_do_end_test() ->
  {Fun, _} = elixir:eval("fn do\n1 + 2\nend"),
  3 = Fun().

function_arg_do_end_test() ->
  {3, _} = elixir:eval("if date do\n1 + 2\nend"),
  {nil, _} = elixir:eval("if date do end").

function_assignment_test() ->
  {_, [{a, Fun}]} = elixir:eval("a = fn do: 1 + 2"),
  3 = Fun().

function_with_args_test() ->
  {Fun, _} = elixir:eval("fn(a, b, do: a + b)"),
  3 = Fun(1,2).

function_with_kv_args_test() ->
  {Fun, _} = elixir:eval("fn(a, [other: b, another: c], do: a + b + c)"),
  6 = Fun(1,[{another,3},{other,2}]).

function_as_clojure_test() ->
  {_, [{a, Res1}|_]} = elixir:eval("b = 1; a = fn { b + 2 }"),
  3 = Res1().

%% Function calls

function_call_test() ->
  {3, _} = elixir:eval("x = fn(a,b, do: a + b)\nx.(1,2)").

function_call_without_arg_test() ->
  {3, _} = elixir:eval("x = fn do: 2 + 1\nx.()").

function_call_do_end_test() ->
  {[1,[{do,2},{else,3}]], _} = elixir:eval("x = fn(a, b){ [a, b] }\nx.(1) do\n2\nelse: 3\nend").

function_call_with_assignment_test() ->
  {3, [{a,_},{c, 3}]} = elixir:eval("a = fn(x){ x + 2 }; c = a.(1)").

function_calls_with_multiple_expressions_test() ->
  {26, _} = elixir:eval("a = fn(a, b){ a + b }; a.((3 + 4 - 1), (2 * 10))").

function_calls_with_multiple_args_with_line_breaks_test() ->
  {5, _} = elixir:eval("a = fn(a, b){ a + b }; a.(\n3,\n2\n)").

function_calls_with_parenthesis_test() ->
  {3, [{a,_},{b,1}]} = elixir:eval("a = fn(x){ x + 2 }.(b = 1)").
