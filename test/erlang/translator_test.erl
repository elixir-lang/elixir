-module(translator_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Forms) ->
  { Result, _ } = eval(Forms, []),
  Result.

eval(Forms, Binding) ->
  { Transformed, FinalScope } = elixir_translator:translate(Forms, #elixir_scope{}),
  io:format("~p~n", [Transformed]),
  { value, Result, NewBinding } = erl_eval:exprs(Transformed, []),
  { Result, NewBinding }.

%% Assignment Operator

assignment_test() ->
  {13, [{a,13}]} = eval([{'=', 1, [{a, 1, false},13]}], []).

assigning_twice_test() ->
  23 = eval([{'=', 1, [{a, 1, false},13]}, {'=', 2, [{a, 2, false}, {'+', 2, [{a, 2, false}, 10]}]}]).

assignment_match_test() ->
  ?assertError({badmatch, 2}, eval([{'=', 1, [13, 2]}])).

%% Lists

list_test() ->
  [] = eval([{'[]',1,[]}]),
  [1,2,3] = eval([{'[]',1,[1,2,3]}]),
  [1,2,3|4] = eval([{'[]',1,[1,2,{'|',1,[3,4]}]}]).

%% Ifs

if_do_test() ->
  example = eval([{ 'if', 1, [true, { '{}', 1, [{do, 1, example}] }] }]).

if_do_else_test() ->
  failed = eval([{ 'if', 1, [false, { '{}', 1, [{do, 1, example},{else, 1, failed}] }] }]).

if_do_else_elsif_test() ->
  ok  = eval([{ 'if', 1, [false, { '{}', 1, [{do, 1, example},{else, 1, failed},{elsif, 1, [true, ok]}] }] }]),
  nil = eval([{ 'if', 1, [false, { '{}', 1, [{do, 1, example},{else, 1, failed},{elsif, 1, [true]}] }] }]),
  10  = eval([{ 'if', 1, [false, { '{}', 1, [{do, 1, example},{else, 1, failed},{elsif, 1, [false]},{elsif, 1, [true, 10]}] }] }]).

if_vars_test() ->
  {true,[{'X0',1},{foo,1}]} = eval([{ 'if', 1, [{'=', 1, [{foo,1,false},1]}, { '{}', 1, [{do,1,true},{else,1,false}] }] }], []).

%% Functions

functions_test() ->
  Keywords = { '{}', 1, [{do, 1, { '+', 1, [{ x, 1, false }, { y, 1, false }]}}] },
  Args = { '[]', 1, [{ x, 1, false }, { y, 1, false }] },
  Fun = eval([{ fn, 1, [Args, Keywords] }]),
  3 = Fun(1, 2).

%% Literals

atoms_test() ->
  atom = eval([atom]).

integer_test() ->
  1 = eval([1]).

float_test() ->
  2.0 = eval([2.0]).

%% Math Operators

addition_test() ->
  3 = eval([{'+', 1, [1, 2]}]).

subtraction_test() ->
  1 = eval([{'-', 1, [2, 1]}]).

multiplication_test() ->
  6 = eval([{'*', 1, [2, 3]}]).

division_test() ->
  1.5 = eval([{'/', 1, [3, 2]}]).

% Unary operators

plus_test() ->
  1 = eval([{'+', 1, [1]}]),
  1.2 = eval([{'+', 1, [1.2]}]),
  1.2 = eval([{'+', 1, [{'+', 1, [0.2, 1.0]}]}]).

minus_test() ->
  -1 = eval([{'-', 1, [1]}]),
  -1.2 = eval([{'-', 1, [1.2]}]),
  -1.2 = eval([{'-', 1, [{'+', 1, [0.2, 1.0]}]}]).

%% Short-circuit operators

andand_test() ->
  true  = eval([{'&&', 1, [true, true]}]),
  false = eval([{'&&', 1, [true, false]}]),
  false = eval([{'&&', 1, [false, true]}]),
  false = eval([{'&&', 1, [false, false]}]),
  false = eval([{'&&', 1, [false, {error, 1, [omg]}]}]).

oror_test() ->
  true  = eval([{'||', 1, [true, true]}]),
  true  = eval([{'||', 1, [true, false]}]),
  true  = eval([{'||', 1, [false, true]}]),
  false = eval([{'||', 1, [false, false]}]),
  true  = eval([{'||', 1, [true, {error, 1, [omg]}]}]).

%% Method calls

local_call_test() ->
  42.0 = eval([{float, 1, [42]}]).

%% Expressions

expressions_test() ->
  nil = eval([[]]),
  3   = eval([1, 2, 3]),
  5   = eval([{'+', 1, [{'+', 1, [1, 2]}, 2]}]),
  7   = eval([{'+', 1, [[{'+', 1, [1, 2]}, {'+', 1, [2, 3]}], 2]}]).
