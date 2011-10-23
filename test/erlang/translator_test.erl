-module(translator_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Forms) ->
  { Transformed, FinalScope } = elixir_translator:translate(Forms, #elixir_scope{}),
  { value, Result, _ } = erl_eval:exprs(Transformed, []),
  Result.

%% LITERALS

atoms_test() ->
  atom = eval([atom]).

integer_test() ->
  1 = eval([1]).

float_test() ->
  2.0 = eval([2.0]).

%% Operators

addition_test() ->
  3 = eval([{'+', 1, 1, 2}]).

subtraction_test() ->
  1 = eval([{'-', 1, 2, 1}]).

multiplication_test() ->
  6 = eval([{'*', 1, 2, 3}]).

division_test() ->
  1.5 = eval([{'/', 1, 3, 2}]).

%% Expressions

expressions_test() ->
  3 = eval([1, 2, 3]).