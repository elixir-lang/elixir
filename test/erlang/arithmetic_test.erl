-module(arithmetic_test).
-include_lib("eunit/include/eunit.hrl").

separator_test() ->
  {334,[]} = elixir:eval("3_34"),
  {600,[]} = elixir:eval("2_00+45_5-5_5").

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

integer_divdiv_test() ->
  {0,[]} = elixir:eval("1 div 2"),
  {2,[]} = elixir:eval("4 div 2").

integer_rem_test() ->
  {1,[]} = elixir:eval("1 rem 2"),
  {0,[]} = elixir:eval("4 rem 2").

integer_rem_div_variables_test() ->
  {1,[{'rem',1}]} = elixir:eval("rem = 1\nrem rem 2"),
  {2,[{'div',4}]} = elixir:eval("div = 4\ndiv div 2").

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
  {2,[]}    = elixir:eval("- 1 * - 2").

integer_eol_test() ->
  {3,[]} = elixir:eval("1 +\n2"),
  {2,[]} = elixir:eval("1 *\n2"),
  {8,[]} = elixir:eval("1 + 2\n3 + 5"),
  {8,[]} = elixir:eval("1 + 2\n\n\n3 + 5"),
  {8,[]} = elixir:eval("1 + 2;\n\n3 + 5"),
  {8,[]} = elixir:eval("1 + (\n2\n) + 3 + 2"),
  {8,[]} = elixir:eval("1 + (\n\n  2\n\n) + 3 + 2"),
  ?assertError({badsyntax, _}, elixir:eval("1 + 2;\n;\n3 + 5")),
  ?assertError({badsyntax, _}, elixir:eval(";1 + 2")).

float_with_parens_and_unary_test() ->
  {-21.0,[]} = elixir:eval("-3.0 * (5 + 2)"),
  {25.0,[]}  = elixir:eval("(2 + 3.0) * 5"),
  {0.25,[]}  = elixir:eval("4 / (11.0 + 5)").

sum_as_explicit_method_call_test() ->
  {3, [{a,1},{b,2}]} = elixir:eval("(a = 1).+(b = 2)").

operators_precedence_test() ->
  F = fun() ->
    {3,[]} = elixir:eval("module Foo; def length; 1; end; end\n1 + Foo.length + 1"),
    {3,[]} = elixir:eval("1 + Foo.length+1"),
    {2,[]} = elixir:eval("module Bar; def length(x); 1; end; end\n1 + Bar.length +1")
  end,
  test_helper:run_and_remove(F, ['Foo', 'Bar']).