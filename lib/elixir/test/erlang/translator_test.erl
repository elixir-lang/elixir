-module(translator_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Forms) ->
  { Result, _ } = eval(Forms, []),
  Result.

eval(Forms, Binding) ->
  { Transformed, _ } = elixir_translator:translate(Forms, elixir:scope_for_eval([])),
  { value, Result, NewBinding } = erl_eval:exprs(Transformed, Binding),
  { Result, NewBinding }.

%% Assignment Operator

assignment_test() ->
  {13, [{a,13}]} = eval([{'=', [{line,1}], [{a, [{line,1}], nil},13]}], []).

assigning_twice_test() ->
  23 = eval([{'=', [{line,1}], [{a, [{line,1}], nil},13]}, {'=', [{line,2}], [{a, [{line,2}], nil}, {'+', [{line,2}], [{a, [{line,2}], nil}, 10]}]}]).

assignment_match_test() ->
  ?assertError({badmatch, 2}, eval([{'=', [{line,1}], [13, 2]}])).

%% Aliases

single_alias_test() ->
  {'Elixir.Foo', _} = eval([{'__aliases__', [{line,1}], ['Foo']}], []).

%% Containers

tuple_test() ->
  {} = eval([{'{}',[{line,1}],[]}]),
  {1,2,3} = eval([{'{}',[{line,1}],[1,2,3]}]).

%% Ifs

if_do_test() ->
  example = eval([{ 'if', [{line,1}], [true, [{do, example}]] }]).

if_do_else_test() ->
  failed = eval([{ 'if', [{line,1}], [false, [{do, example},{else, failed}] ] }]).

if_vars_test() ->
  {true,[{foo,1}]} = eval([{ 'if', [{line,1}], [{'=', [{line,1}], [{foo,[{line,1}],nil},1]}, [{do,true},{else,false}] ] }], []).

%% Literals

list_test() ->
  [] = eval([[]]),
  [1,2,3] = eval([[1,2,3]]),
  [1,2,3,{do,foo}] = eval([[1,2,3,{'{}',[{line,1}],[do,foo]}]]),
  [1,2,3|4] = eval([[1,2,{'|',[{line,1}],[3,4]}]]).

kv_test() ->
  {key, 1} = eval([{key, 1}]).

atoms_test() ->
  atom = eval([atom]).

integer_test() ->
  1 = eval([1]).

float_test() ->
  2.0 = eval([2.0]).

%% Math Operators

addition_test() ->
  3 = eval([{'+', [{line,1}], [1, 2]}]).

subtraction_test() ->
  1 = eval([{'-', [{line,1}], [2, 1]}]).

multiplication_test() ->
  6 = eval([{'*', [{line,1}], [2, 3]}]).

division_test() ->
  1.5 = eval([{'/', [{line,1}], [3, 2]}]).

% Unary operators

plus_test() ->
  1 = eval([{'+', [{line,1}], [1]}]),
  1.2 = eval([{'+', [{line,1}], [1.2]}]),
  1.2 = eval([{'+', [{line,1}], [{'+', [{line,1}], [0.2, 1.0]}]}]).

minus_test() ->
  -1 = eval([{'-', [{line,1}], [1]}]),
  -1.2 = eval([{'-', [{line,1}], [1.2]}]),
  -1.2 = eval([{'-', [{line,1}], [{'+', [{line,1}], [0.2, 1.0]}]}]).

%% Short-circuit operators

andand_test() ->
  true  = eval([{'&&', [{line,1}], [true, true]}]),
  false = eval([{'&&', [{line,1}], [true, false]}]),
  false = eval([{'&&', [{line,1}], [false, true]}]),
  false = eval([{'&&', [{line,1}], [false, false]}]),
  false = eval([{'&&', [{line,1}], [false, {error, [{line,1}], [omg]}]}]).

oror_test() ->
  true  = eval([{'||', [{line,1}], [true, true]}]),
  true  = eval([{'||', [{line,1}], [true, false]}]),
  true  = eval([{'||', [{line,1}], [false, true]}]),
  false = eval([{'||', [{line,1}], [false, false]}]),
  true  = eval([{'||', [{line,1}], [true, {error, [{line,1}], [omg]}]}]).

%% Function calls

local_call_test() ->
  42.0 = eval([{float, [{line,1}], [42]}]).
