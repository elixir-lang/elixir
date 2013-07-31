-module(function_test).
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

function_arg_do_end_test() ->
  {3, _} = eval("if true do\n1 + 2\nend"),
  {nil, _} = eval("if true do end").

function_stab_end_test() ->
  {_, [{a, Fun1}]} = eval("a = fn -> end"),
  nil = Fun1(),
  {_, [{a, Fun2}]} = eval("a = fn() -> end"),
  nil = Fun2(),
  {_, [{a, Fun3}]} = eval("a = fn -> 1 + 2 end"),
  3 = Fun3().

function_stab_many_test() ->
  {_, [{a, Fun}]} = eval("a = fn\n{ :foo, x } -> x\n{ :bar, x } -> x\nend"),
  1 = Fun({ foo, 1 }),
  2 = Fun({ bar, 2 }).

function_stab_inline_test() ->
  {_, [{a, Fun}]} = eval("a = fn { :foo, x } -> x; { :bar, x } -> x end"),
  1 = Fun({ foo, 1 }),
  2 = Fun({ bar, 2 }).

function_with_args_test() ->
  {Fun, _} = eval("fn(a, b) -> a + b end"),
  3 = Fun(1,2).

function_with_kv_args_test() ->
  {Fun, _} = eval("fn(a, [other: b, another: c]) -> a + b + c end"),
  6 = Fun(1,[{other,2}, {another,3}]).

function_as_closure_test() ->
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

function_call_with_a_single_space_test() ->
  {3, _} = eval("a = fn a, b -> a + b end; a. (1,2)"),
  {3, _} = eval("a = fn a, b -> a + b end; a .(1,2)").

function_call_with_spaces_test() ->
  {3, _} = eval("a = fn a, b -> a + b end; a . (1,2)").

function_call_without_assigning_with_spaces_test() ->
  {3, _} = eval("(fn a, b -> a + b end) . (1,2)").

function_call_with_assignment_and_spaces_test() ->
  {3, [{a,_},{c,3}]} = eval("a = fn x -> x + 2 end; c = a . (1)").

function_call_with_multiple_spaces_test() ->
  {3, _} = eval("a = fn a, b -> a + b end; a .         (1,2)").

function_call_with_multiline_test() ->
  {3, _} = eval("a = fn a, b -> a + b end; a .   \n      (1,2)").

function_call_with_tabs_test() ->
  {3, _} = eval("a = fn a, b -> a + b end; a .\n\t(1,2)").

function_call_with_args_and_nested_when_test() ->
  {Fun, _} = eval("fn a, b when a == 1 when b == 2 -> a + b end"),
  3 = Fun(1, 2),
  2 = Fun(0, 2),
  1 = Fun(1, 0),
  ?assertError(function_clause, Fun(0, 0)).

function_call_with_parens_args_and_nested_when_test() ->
  {Fun, _} = eval("fn\n(a, b) when a == 1 when b == 2 -> a + b\nend"),
  3 = Fun(1, 2),
  2 = Fun(0, 2),
  1 = Fun(1, 0),
  ?assertError(function_clause, Fun(0, 0)).

%% Partial application

require_partial_application_test() ->
  { Fun, _ } = eval("List.flatten(&1)"),
  Fun = fun 'Elixir.List':flatten/1.

import_partial_application_test() ->
  { Fun, _ } = eval("is_atom(&1)"),
  Fun = fun erlang:is_atom/1.
