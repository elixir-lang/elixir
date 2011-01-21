-module(match_test).
-include_lib("eunit/include/eunit.hrl").

no_assignment_test() ->
  {[], []} = elixir:eval("").

% Var/assignment test
assignment_test() ->
  {1, [{a, 1}]} = elixir:eval("a = 1").

multiline_assignment_test() ->
  {1, [{a, 1}]} = elixir:eval("a =\n1"),
  {1, [{a, 1}, {b, 1}]} = elixir:eval("a = 1\nb = 1").

multiple_assignment_test() ->
  {1, [{a, 1}, {b, 1}]} = elixir:eval("a = b = 1").

multiple_assignment_with_parens_test() ->
  {1, [{a, 1}, {b, 1}]} = elixir:eval("a = (b = 1)").

multiple_assignment_with_left_parens_test() ->
  {1, [{a, 1}, {b, 1}]} = elixir:eval("(a) = (b = 1)").

multiple_assignment_with_expression_test() ->
  {-4, [{a, -4}, {b, -4}]} = elixir:eval("a = (b = -(2 * 2))").

multiple_assignment_with_binding_expression_test() ->
  {3, [{a, 3}, {b, 1}]} = elixir:eval("a = (b + 2)", [{b, 1}]).

% Atoms match
simple_atom_test() ->
  {foo, [{a, foo}]} = elixir:eval("a = 'foo").

full_atom_test() ->
  {'foo bar', [{a, 'foo bar'}]} = elixir:eval("a = '\"foo bar\"").

atom_match_test() ->
  {foo, []} = elixir:eval("'foo = 'foo"),
  ?assertError({badmatch, _}, elixir:eval("'bar = 'foo")).

atom_match_on_function_test() ->
  {3, _} = elixir:eval("a = -> ('foo, x) x + 1\na('foo, 2)").

% Tuples match
simple_tuple_test() ->
  {{}, _} = elixir:eval("a = {}"),
  {{1,2,3}, _} = elixir:eval("a = {1, 2, 3}"),
  {{1,2,3}, _} = elixir:eval("a = {1, 1 + 1, 3}"),
  {{1,{2},3}, _} = elixir:eval("a = {1, {2}, 3}").

tuple_match_test() ->
  {_, _} = elixir:eval("{1,2,3} = {1, 2, 3}"),
  ?assertError({badmatch, _}, elixir:eval("{1, 3, 2} = {1, 2, 3}")).

tuple_match_on_function_test() ->
  {4, _} = elixir:eval("a = -> ({ 1, 2, x}) x + 1\na({1,2,3})").
