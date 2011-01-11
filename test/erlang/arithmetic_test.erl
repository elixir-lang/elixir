-module(arithmetic_test).
-include_lib("eunit/include/eunit.hrl").

% TODO Arithmetic operations are hardcoded.
% They should instead be implemented using message passing.

% TODO Implement div, res and pow.

integer_sum_test() ->
  ?assertEqual({3,[]}, elixir:eval("1+2")),
  ?assertEqual({6,[]}, elixir:eval("1+2+3")),
  ?assertEqual({6,[]}, elixir:eval("1 + 2 + 3")).

integer_sum_minus_test() ->
  ?assertEqual({-4,[]}, elixir:eval("1-2-3")),
  ?assertEqual({0,[]}, elixir:eval("1+2-3")),
  ?assertEqual({0,[]}, elixir:eval("1 + 2 - 3")).

integer_mult_test() ->
  ?assertEqual({6,[]}, elixir:eval("1*2*3")),
  ?assertEqual({6,[]}, elixir:eval("1 * 2 * 3")).

integer_div_test() ->
  ?assertEqual({0.5,[]}, elixir:eval("1 / 2")),
  ?assertEqual({2.0,[]}, elixir:eval("4 / 2")).

integer_mult_div_test() ->
  ?assertEqual({1.0,[]}, elixir:eval("2*1/2")),
  ?assertEqual({6.0,[]}, elixir:eval("3 * 4 / 2")).

integer_without_parens_test() ->
  ?assertEqual({17,[]}, elixir:eval("3 * 5 + 2")),
  ?assertEqual({17,[]}, elixir:eval("2 + 3 * 5")),
  ?assertEqual({6.0,[]}, elixir:eval("4 / 4 + 5")).

integer_with_parens_test() ->
  ?assertEqual({21,[]}, elixir:eval("3 * (5 + 2)")),
  ?assertEqual({25,[]}, elixir:eval("(2 + 3) * 5")),
  ?assertEqual({0.25,[]}, elixir:eval("4 / (11 + 5)")).

integer_with_unary_test() ->
  ?assertEqual({-1,[]}, elixir:eval("-1")),
  ?assertEqual({1,[]}, elixir:eval("+1")),
  ?assertEqual({-1,[]}, elixir:eval("(-1)")),
  ?assertEqual({1,[]}, elixir:eval("-(1 - 2)")),
  ?assertEqual({1,[]}, elixir:eval("+1")),
  ?assertEqual({3,[]}, elixir:eval("+ 1 + 2")),
  ?assertEqual({-3,[]}, elixir:eval("- 1 + - 2")),
  ?assertEqual({2,[]}, elixir:eval("- 1 * - 2")),
  ?assertEqual({-0.5,[]}, elixir:eval("+ 1 / - 2")).

integer_eol_test() ->
  ?assertEqual({8,[]}, elixir:eval("1 + 2\n3 + 5")),
  ?assertEqual({8,[]}, elixir:eval("1 + 2\n\n\n3 + 5")),
  ?assertEqual({8,[]}, elixir:eval("1 + 2;\n;;\n;3 + 5")),
  ?assertEqual({8,[]}, elixir:eval("1 + (\n2\n) + 3 + 2")),
  ?assertEqual({8,[]}, elixir:eval("1 + (\n\n  2\n\n) + 3 + 2")).

float_with_parens_and_unary_test() ->
  ?assertEqual({-21.0,[]}, elixir:eval("-3.0 * (5 + 2)")),
  ?assertEqual({25.0,[]}, elixir:eval("(2 + 3.0) * 5")),
  ?assertEqual({0.25,[]}, elixir:eval("4 / (11.0 + 5)")).