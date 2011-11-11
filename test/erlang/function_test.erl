-module(function_test).
-include_lib("eunit/include/eunit.hrl").

function_do_end_test() ->
  {Fun, _} = elixir:eval("fn do\n1 + 2\nend"),
  3 = Fun().

function_arg_do_end_test() ->
  {3, _} = elixir:eval("if date do\n1 + 2\nend").

function_assignment_test() ->
  {_, [{a, Fun}]} = elixir:eval("a = fn do: 1 + 2"),
  3 = Fun().

function_with_args_test() ->
  {Fun, _} = elixir:eval("fn [a,b], do: a + b"),
  3 = Fun(1,2).

% function_assignment_multiline_test() ->
%   {_, [{a, Res2}]} = elixir:eval("a = do \nx = 1\nx + 2\n end"),
%   3 = Res2(),
%   {_, [{a, Res4}]} = elixir:eval("a = do (y) \nx = 1\nx + y\n end"),
%   3 = Res4(2).
% 
% function_assignment_with_assignment_test() ->
%   {_, [{a, Res1}]} = elixir:eval("a = -> b = 3"),
%   3 = Res1().
% 
% function_assignment_with_empty_args_test() ->
%   {_, [{a, Res1}]} = elixir:eval("a = -> () 1 + 2"),
%   3 = Res1(),
%   {_, [{a, Res3}]} = elixir:eval("a = -> (\n) (1 + 2)"),
%   3 = Res3().
% 
% function_assignment_with_args_test() ->
%   {_, [{a, Res1}]} = elixir:eval("a = -> (x,y) x + y"),
%   3 = Res1(1,2),
%   {_, [{a, Res2}]} = elixir:eval("a = do (x,y); x + y; end"),
%   3 = Res2(1,2),
%   {_, [{a, Res3}]} = elixir:eval("a = -> (\nx,\ny\n) x + y"),
%   3 = Res3(1,2).
% 
% function_nested_assignment_test() ->
%   {_, [{a, Res1}]} = elixir:eval("a = -> -> 1 + 2"),
%   ResF1 = Res1(),
%   3 = ResF1().
% 
% function_assignment_new_line_test() ->
%   {_, [{a, Res2}]} = elixir:eval("a = do\n1 + 2\nend"),
%   3 = Res2().
% 
% function_nested_assignment_new_line_test() ->
%   {_, [{a, Res1}]} = elixir:eval("a = -> do\n1 + 2\nend "),
%   ResF1 = Res1(),
%   3 = ResF1(),
%   {_, [{a, Res2}]} = elixir:eval("a = do\n-> 1 + 2\nend "),
%   ResF2 = Res2(),
%   3 = ResF2().
% 
% function_as_clojure_test() ->
%   {_, [{a, Res1}|_]} = elixir:eval("b = 1; a = -> b + 2"),
%   3 = Res1(),
%   {_, [{a, Res2}|_]} = elixir:eval("b = 1; a = do\nb + 2\nend"),
%   3 = Res2().
% 
% %% Function calls
% function_calls_test() ->
%   {3, _} = elixir:eval("b = 1; a = -> 2 + b; a.()").
% 
% function_calls_with_arg_test() ->
%   {3, _} = elixir:eval("b = 1; a = -> (a) a + b; a.(2)").
% 
% function_call_with_assignment_test() ->
%   {3, [{a,_},{c, 3}]} = elixir:eval("a = -> (x) x + 2; c = a.(1)").
% 
% function_call_inside_another_function_test() ->
%   {1, _} = elixir:eval("a = -> (x) x + 2; b = -> a.(1) - 2; b.()").
% 
% function_calls_with_multiple_args_test() ->
%   {5, _} = elixir:eval("a = -> (a, b) a + b; a.(3, 2)").
% 
% function_calls_with_multiple_expressions_test() ->
%   {26, _} = elixir:eval("a = -> (a, b) a + b; a.((3 + 4 - 1), (2 * 10))").
% 
% function_calls_with_multiple_args_with_line_breaks_test() ->
%   {5, _} = elixir:eval("a = -> (a, b) a + b; a.(\n3,\n2\n)").
% 
% function_calls_with_parenthesis_test() ->
%   {3, [{a,_},{b,1}]} = elixir:eval("(a = -> (x) x + 2).(b = 1)").