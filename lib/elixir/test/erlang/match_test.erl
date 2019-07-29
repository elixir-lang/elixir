-module(match_test).
-include_lib("eunit/include/eunit.hrl").

eval(Content) -> eval(Content, []).

eval(Content, Initial) ->
  {Value, Binding, _, _} =
    elixir:eval_forms(elixir:'string_to_quoted!'(Content, 1, <<"nofile">>, []), Initial, []),
  {Value, Binding}.

no_assignment_test() ->
  {nil, []} = eval("").

% Var/assignment test
arithmetic_test() ->
  ?assertError({badmatch, _}, eval("-1 = 1")).

assignment_test() ->
  {1, [{a, 1}]} = eval("a = 1").

not_single_assignment_test() ->
  {2, [{a, 2}]} = eval("a = 1\na = 2\na"),
  {1, [{a, 1}]} = eval("{a, a} = {1, 1}\na"),
  {2, [{a, 2}]} = eval("a = 1\n{^a, a} = {1, 2}\na"),
  ?assertError({badmatch, _}, eval("{a, a} = {1, 2}")),
  ?assertError({badmatch, _}, eval("{1 = a, a} = {1, 2}")),
  ?assertError({badmatch, _}, eval("{a = 1, a} = {1, 2}")),
  ?assertError({badmatch, _}, eval("a = 0;{a, a} = {1, 2}")),
  ?assertError({badmatch, _}, eval("a = 0;{1 = a, a} = {1, 2}")),
  ?assertError({badmatch, _}, eval("a = 1\n^a = 2")).

duplicated_assignment_on_module_with_tuple_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef v({a, _left}, {a, _right}), do: a\nend"),
    {1, _} = eval("Foo.v({1, :foo}, {1, :bar})"),
    ?assertError(function_clause, eval("Foo.v({1, :foo}, {2, :bar})"))
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).

duplicated_assignment_on_module_with_list_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef v([ a, _left ], [ a, _right ]), do: a\nend"),
    {1, _} = eval("Foo.v([ 1, :foo ], [ 1, :bar ])"),
    ?assertError(function_clause, eval("Foo.v([ 1, :foo ], [ 2, :bar ])"))
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).

multiline_assignment_test() ->
  {1, [{a, 1}]} = eval("a =\n1"),
  {1, [{a, 1}, {b, 1}]} = eval("a = 1\nb = 1").

multiple_assignment_test() ->
  {1, [{a, 1}, {b, 1}]} = eval("a = b = 1").

multiple_assignment_with_parens_test() ->
  {1, [{a, 1}, {b, 1}]} = eval("a = (b = 1)").

multiple_assignment_with_left_parens_test() ->
  {1, [{a, 1}, {b, 1}]} = eval("(a) = (b = 1)").

multiple_assignment_with_expression_test() ->
  {-4, [{a, -4}, {b, -4}]} = eval("a = (b = -(2 * 2))").

multiple_assignment_with_binding_expression_test() ->
  {3, [{a, 3}, {b, 1}]} = eval("a = (2 + b)", [{b, 1}]).

underscore_assignment_test() ->
  {1, []} = eval("_ = 1").

assignment_precedence_test() ->
  {_, [{x, {'__block__', _, [1, 2, 3]}}]} = eval("x = quote do\n1\n2\n3\nend").

% Tuples match
simple_tuple_test() ->
  {{}, _} = eval("a = {}"),
  {{1, 2, 3}, _} = eval("a = {1, 2, 3}"),
  {{1, 2, 3}, _} = eval("a = {1, 1 + 1, 3}"),
  {{1, {2}, 3}, _} = eval("a = {1, {2}, 3}").

tuple_match_test() ->
  {_, _} = eval("{1, 2, 3} = {1, 2, 3}"),
  ?assertError({badmatch, _}, eval("{1, 3, 2} = {1, 2, 3}")).

% Lists match
simple_list_test() ->
  {[], _} = eval("a = []"),
  {[1, 2, 3], _} = eval("a = [1, 2, 3]"),
  {[1, 2, 3], _} = eval("a = [1, 1 + 1, 3]"),
  {[1, [2], 3], _} = eval("a = [1, [2], 3]"),
  {[1, {2}, 3], _} = eval("a = [1, {2}, 3]").

list_match_test() ->
  {_, _} = eval("[1, 2, 3] = [1, 2, 3]"),
  ?assertError({badmatch, _}, eval("[1, 3, 2] = [1, 2, 3]")).

list_vars_test() ->
  {[3, 1], [{x, 3}]} = eval("x = 1\n[x = x + 2, x]").

head_and_tail_test() ->
  {_, [{h, 1}, {t, [2, 3]}]} = eval("[h | t] = [1, 2, 3]"),
  {_, [{h, 2}, {t, [3]}]} = eval("[1, h | t] = [1, 2, 3]"),
  {_, [{t, [3]}]} = eval("[1, 2 | t] = [1, 2, 3]"),
  {_, [{h, 1}]} = eval("[h | [2, 3]] = [1, 2, 3]"),
  {_, [{t, [2, 3]}]} = eval("[+1 | t] = [1, 2, 3]"),
  ?assertError({badmatch, _}, eval("[2, h | t] = [1, 2, 3]")).

% Keyword match

orrdict_match_test() ->
  {[{a, 1}, {b, 2}], _} = eval("a = [a: 1, b: 2]").

% Function match

function_clause_test() ->
  F = fun() ->
    eval("defmodule Foo do\ndef a([{_k, _}=e | _]), do: e\nend"),
    {{foo, bar}, _} = eval("Foo.a([{:foo, :bar}])")
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo']).
