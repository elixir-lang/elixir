-module(ordered_dict_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% dict_find(Key, Dict) ->
%   Struct = Dict#elixir_orddict__.struct,
%   orddict:find(Key, Struct).
% 
% simple_dict_test() ->
%   {Dict, _} = elixir:eval("{ 'a: 1 }"),
%   {ok, 1} = dict_find(a, Dict),
%   error = dict_find(b, Dict).
% 
% dict_with_expressions_test() ->
%   {Dict, _} = elixir:eval("{ 'a: 1, 1 + 2: 3 + 6 }"),
%   {ok, 1} = dict_find(a, Dict),
%   {ok, 9} = dict_find(3, Dict).
% 
% empty_dict_test() ->
%   {Dict, _} = elixir:eval("{}"),
%   error = dict_find(b, Dict).
% 
% dict_with_variables_test() ->
%   {Dict, _} = elixir:eval("a = 'a\nb = 1\n{ a: b, 'b: 2}"),
%   {ok, 1} = dict_find(a, Dict),
%   {ok, 2} = dict_find(b, Dict),
%   error = dict_find(c, Dict).
% 
% dict_with_assignment_test() ->
%   {Dict, [{a, a}, {b, 1}]} = elixir:eval("{ (a = 'a): (b = 1) }"),
%   {ok, 1} = dict_find(a, Dict),
%   error = dict_find(b, Dict).
% 
% dict_in_method_calls_test() ->
%   F = fun() ->
%     elixir:eval("module Bar\nmixin self\ndef foo(x);x;end\ndef bar(x,y); [x,y]; end\nend"),
%     {Dict1,[]} = elixir:eval("Bar.foo('a: 1)"),
%     {ok, 1} = dict_find(a, Dict1),
%     {Dict2,[]} = elixir:eval("Bar.foo 'a: 1"),
%     {ok, 1} = dict_find(a, Dict2),
%     {[1, Dict3],[]} = elixir:eval("Bar.bar(1, 'a: 1)"),
%     {ok, 1} = dict_find(a, Dict3),
%     {[1, Dict4],[]} = elixir:eval("Bar.bar 1, 'a: 1"),
%     {ok, 1} = dict_find(a, Dict4),
%     {[Dict5, Dict6],[]} = elixir:eval("Bar.bar({'b: 2}, 'a: 1)"),
%     {ok, 2} = dict_find(b, Dict5),
%     {ok, 1} = dict_find(a, Dict6),
%     {[Dict7, Dict8],[]} = elixir:eval("Bar.bar {'b: 2}, 'a: 1"),
%     {ok, 2} = dict_find(b, Dict7),
%     {ok, 1} = dict_find(a, Dict8)
%   end,
%   test_helper:run_and_remove(F, ['Bar']).
% 
% dict_fold_test() ->
%   { List, _ } = elixir:eval("{'a: 1, 'b: 2}.fold [], -> (k, v, acc) [{k,v}|acc]"),
%   true = lists:member({a,1}, List),
%   true = lists:member({b,2}, List).
% 
% dict_inspect_test() ->
%   { String, _ } = elixir:eval("{'a: 1, 'b: 2}.inspect"),
%   "{'a: 1, 'b: 2}" = binary_to_list(test_helper:unpack_string(String)).