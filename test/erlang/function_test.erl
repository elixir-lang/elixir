-module(function_test).
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

function_do_end_test() ->
  {Fun, _} = eval("fn do\n1 + 2\nend"),
  3 = Fun().

function_arg_do_end_test() ->
  {3, _} = eval("if date do\n1 + 2\nend"),
  {nil, _} = eval("if date do end").

function_assignment_test() ->
  {_, [{a, Fun}]} = eval("a = fn do: 1 + 2"),
  3 = Fun().

function_stab_end_test() ->
  {_, [{a, Fun}]} = eval("a = fn -> 1 + 2 end"),
  3 = Fun().

function_with_args_test() ->
  {Fun, _} = eval("fn(a, b, do: a + b)"),
  3 = Fun(1,2).

function_with_kv_args_test() ->
  {Fun, _} = eval("fn(a, [other: b, another: c], do: a + b + c)"),
  6 = Fun(1,[{another,3},{other,2}]).

function_as_clojure_test() ->
  {_, [{a, Res1}|_]} = eval("b = 1; a = fn do: b + 2"),
  3 = Res1().

function_apply_test() ->
  {3,_} = eval("a = fn do: 3; apply a, []").

function_apply_with_args_test() ->
  {3,_} = eval("a = fn(b) -> b + 2 end; apply a, [1]").

function_apply_and_clojure_test() ->
  {3,_} = eval("b = 1; a = fn -> b + 2 end; apply a, []").

%% Function calls

function_call_test() ->
  {3, _} = eval("x = fn(a,b, do: a + b)\nx.(1,2)").

function_call_without_arg_test() ->
  {3, _} = eval("x = fn do: 2 + 1\nx.()").

function_call_do_end_test() ->
  {[1,[{do,2},{else,3}]], _} = eval("x = fn(a, b, do: [a,b])\nx.(1) do\n2\nelse: 3\nend").

function_call_with_assignment_test() ->
  {3, [{a,_},{c, 3}]} = eval("a = fn(x, do: x + 2); c = a.(1)").

function_calls_with_multiple_expressions_test() ->
  {26, _} = eval("a = fn(a, b, do: a + b); a.((3 + 4 - 1), (2 * 10))").

function_calls_with_multiple_args_with_line_breaks_test() ->
  {5, _} = eval("a = fn(a, b, do: a + b); a.(\n3,\n2\n)").

function_calls_with_parenthesis_test() ->
  {3, [{a,_},{b,1}]} = eval("a = fn(x, do: x + 2).(b = 1)").
