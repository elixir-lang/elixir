-module(translator_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Forms) ->
  { Transformed, FinalScope } = elixir_translator:translate(Forms, #elixir_scope{}),
  io:format("~p~n", [Transformed]),
  { value, Result, _ } = erl_eval:exprs(Transformed, []),
  Result.

%% Literals

atoms_test() ->
  atom = eval([atom]).

integer_test() ->
  1 = eval([1]).

float_test() ->
  2.0 = eval([2.0]).

%% Math Operators

addition_test() ->
  3 = eval([{'+', 1, 1, 2}]).

subtraction_test() ->
  1 = eval([{'-', 1, 2, 1}]).

multiplication_test() ->
  6 = eval([{'*', 1, 2, 3}]).

division_test() ->
  1.5 = eval([{'/', 1, 3, 2}]).

%% Short-circuit operators

andand_test() ->
  true  = eval([{'&&', 1, true, true}]),
  false = eval([{'&&', 1, true, false}]),
  false = eval([{'&&', 1, false, true}]),
  false = eval([{'&&', 1, false, false}]),
  false = eval([{'&&', 1, false, {error, 1, [omg]}}]).

oror_test() ->
  true  = eval([{'||', 1, true, true}]),
  true  = eval([{'||', 1, true, false}]),
  true  = eval([{'||', 1, false, true}]),
  false = eval([{'||', 1, false, false}]),
  true  = eval([{'||', 1, true, {error, 1, [omg]}}]).

%% Method calls

local_call_test() ->
  42.0 = eval([{float, 1, [42]}]).

%% Expressions

expressions_test() ->
  nil = eval([[]]),
  3   = eval([1, 2, 3]),
  5   = eval([{'+', 1, [{'+', 1, 1, 2}], 2}]),
  7   = eval([{'+', 1, [{'+', 1, 1, 2}, {'+', 1, 2, 3}], 2}]).
