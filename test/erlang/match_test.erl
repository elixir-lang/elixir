-module(match_test).
-include_lib("eunit/include/eunit.hrl").

% TODO Handle parsing of empty strings. For example:
% no_assignment_test() ->
%   ?assertEqual({[], []}, elixir:eval("")).

assignment_test() ->
  ?assertEqual({1, [{a, 1}]}, elixir:eval("a = 1")).

multiline_assignment_test() ->
  ?assertEqual({1, [{a, 1}]}, elixir:eval("a =\n1")),
  ?assertEqual({1, [{a, 1}, {b, 1}]}, elixir:eval("a = 1\nb = 1")).

multiple_assignment_test() ->
  ?assertEqual({1, [{a, 1}, {b, 1}]}, elixir:eval("a = b = 1")).

multiple_assignment_with_parens_test() ->
  ?assertEqual({1, [{a, 1}, {b, 1}]}, elixir:eval("a = (b = 1)")).

multiple_assignment_with_left_parens_test() ->
  ?assertEqual({1, [{a, 1}, {b, 1}]}, elixir:eval("(a) = (b = 1)")).

multiple_assignment_with_expression_test() ->
  ?assertEqual({-4, [{a, -4}, {b, -4}]}, elixir:eval("a = (b = -(2 * 2))")).

multiple_assignment_with_binding_expression_test() ->
  ?assertEqual({3, [{a, 3}, {b, 1}]}, elixir:eval("a = (b + 2)", [{b, 1}])).

