-module(arithmetic_test).
-include_lib("eunit/include/eunit.hrl").

% TODO Arithmetic operations are hardcoded.
% They should instead be implemented using message passing.

% TODO Implement div, res and pow.

integer_sum_test() ->
  {3,[]} = elixir:eval("1+2"),
  {6,[]} = elixir:eval("1+2+3"),
  {6,[]} = elixir:eval("1 + 2 + 3").

integer_sum_minus_test() ->
  {-4,[]} = elixir:eval("1-2-3"),
  {0,[]}  = elixir:eval("1+2-3"),
  {0,[]}  = elixir:eval("1 + 2 - 3").

integer_mult_test() ->
  {6,[]} = elixir:eval("1*2*3"),
  {6,[]} = elixir:eval("1 * 2 * 3").

integer_div_test() ->
  {0.5,[]} = elixir:eval("1 / 2"),
  {2.0,[]} = elixir:eval("4 / 2").

integer_mult_div_test() ->
  {1.0,[]} = elixir:eval("2*1/2"),
  {6.0,[]} = elixir:eval("3 * 4 / 2").

integer_without_parens_test() ->
  {17,[]}  = elixir:eval("3 * 5 + 2"),
  {17,[]}  = elixir:eval("2 + 3 * 5"),
  {6.0,[]} = elixir:eval("4 / 4 + 5").

integer_with_parens_test() ->
  {21,[]}   = elixir:eval("3 * (5 + 2)"),
  {25,[]}   = elixir:eval("(2 + 3) * 5"),
  {0.25,[]} = elixir:eval("4 / (11 + 5)").

integer_with_unary_test() ->
  {-1,[]}   = elixir:eval("-1"),
  {1,[]}    = elixir:eval("+1"),
  {-1,[]}   = elixir:eval("(-1)"),
  {1,[]}    = elixir:eval("-(1 - 2)"),
  {1,[]}    = elixir:eval("+1"),
  {3,[]}    = elixir:eval("+ 1 + 2"),
  {-3,[]}   = elixir:eval("- 1 + - 2"),
  {2,[]}    = elixir:eval("- 1 * - 2"),
  {-0.5,[]} = elixir:eval("+ 1 / - 2").

integer_eol_test() ->
  {8,[]} = elixir:eval("1 + 2\n3 + 5"),
  {8,[]} = elixir:eval("1 + 2\n\n\n3 + 5"),
  {8,[]} = elixir:eval("1 + 2;\n\n3 + 5"),
  {8,[]} = elixir:eval("1 + (\n2\n) + 3 + 2"),
  {8,[]} = elixir:eval("1 + (\n\n  2\n\n) + 3 + 2"),
  ?assertError({badmatch, _}, elixir:eval("1 + 2;\n;\n3 + 5")),
  ?assertError({badmatch, _}, elixir:eval(";1 + 2")).

float_with_parens_and_unary_test() ->
  {-21.0,[]} = elixir:eval("-3.0 * (5 + 2)"),
  {25.0,[]}  = elixir:eval("(2 + 3.0) * 5"),
  {0.25,[]}  = elixir:eval("4 / (11.0 + 5)").