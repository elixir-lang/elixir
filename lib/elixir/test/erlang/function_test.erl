-module(function_test).
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, elixir_transform}).

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

function_arg_do_end_test() ->
  {3, _} = eval("if date do\n1 + 2\nend"),
  {nil, _} = eval("if date do end").

function_stab_end_test() ->
  {_, [{a, Fun}]} = eval("a = fn -> 1 + 2 end"),
  3 = Fun().

function_with_args_test() ->
  {Fun, _} = eval("fn(a, b) -> a + b end"),
  3 = Fun(1,2).

function_with_kv_args_test() ->
  {Fun, _} = eval("fn(a, [other: b, another: c]) -> a + b + c end"),
  6 = Fun(1,[{other,2}, {another,3}]).

function_as_clojure_test() ->
  {_, [{a, Res1}|_]} = eval("b = 1; a = fn -> b + 2 end"),
  3 = Res1().

function_apply_test() ->
  {3,_} = eval("a = fn -> 3 end; apply a, []").

function_apply_with_args_test() ->
  {3,_} = eval("a = fn b -> b + 2 end; apply a, [1]").

function_apply_and_clojure_test() ->
  {3,_} = eval("b = 1; a = fn -> b + 2 end; apply a, []").

function_parens_test() ->
  {0,_} = eval("(fn() -> 0 end).()"),
  {1,_} = eval("(fn(1) -> 1 end).(1)"),
  {3,_} = eval("(fn(1, 2) -> 3 end).(1, 2)"),

  {0,_} = eval("(fn () -> 0 end).()"),
  {1,_} = eval("(fn (1) -> 1 end).(1)"),
  {3,_} = eval("(fn (1, 2) -> 3 end).(1, 2)").

function_macro_parens_test() ->
  {0,_} = eval("(function do () -> 0 end).()"),
  {1,_} = eval("(function do 1 -> 1 end).(1)"),
  {3,_} = eval("(function do 1, 2 -> 3 end).(1, 2)"),
  {1,_} = eval("(function do (1) -> 1 end).(1)"),
  {3,_} = eval("(function do (1, 2) -> 3 end).(1, 2)").

%% Function calls

function_call_test() ->
  {3, _} = eval("x = fn a, b -> a + b end\nx.(1,2)").

function_call_without_arg_test() ->
  {3, _} = eval("x = fn -> 2 + 1 end\nx.()").

function_call_do_end_test() ->
  {[1,[{do,2},{else,3}]], _} = eval("x = fn a, b -> [a,b] end\nx.(1) do\n2\nelse 3\nend").

function_call_with_assignment_test() ->
  {3, [{a,_},{c, 3}]} = eval("a = fn x -> x + 2 end; c = a.(1)").

function_calls_with_multiple_expressions_test() ->
  {26, _} = eval("a = fn a, b -> a + b end; a.((3 + 4 - 1), (2 * 10))").

function_calls_with_multiple_args_with_line_breaks_test() ->
  {5, _} = eval("a = fn a, b -> a + b end; a.(\n3,\n2\n)").

function_calls_with_parenthesis_test() ->
  {3, [{a,_},{b,1}]} = eval("a = (fn x -> x + 2 end).(b = 1)").

%% Partial application

require_partial_application_test() ->
  { Fun, _ } = eval("List.flatten(&1)"),
  Fun = fun 'Elixir.List':flatten/1.

import_partial_application_test() ->
  { Fun, _ } = eval("is_atom(&1)"),
  Fun = fun erlang:is_atom/1.