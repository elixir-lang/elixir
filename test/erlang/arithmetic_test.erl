-module(arithmetic_test).
-include_lib("eunit/include/eunit.hrl").

separator_test() ->
  {334,[]} = elixir:eval("3_34"),
  {600,[]} = elixir:eval("2_00+45_5-5_5").

integer_sum_test() ->
  {3,[]} = elixir:eval("1+2"),
  {6,[]} = elixir:eval("1+2+3"),
  {6,[]} = elixir:eval("1+2 +3"),
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
  {21,[]}   = elixir:eval("3 * (((5 + (2))))"),
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
  {3,[]} = elixir:eval(";1 + 2"),
  ?assertError({badsyntax, _}, elixir:eval("1 + 2;\n;\n3 + 5")).

float_with_parens_and_unary_test() ->
  {-21.0,[]} = elixir:eval("-3.0 * (5 + 2)"),
  {25.0,[]}  = elixir:eval("(2 + 3.0) * 5"),
  {0.25,[]}  = elixir:eval("4 / (11.0 + 5)").

op_call_test() ->
  {3, []} = elixir:eval("+(1, 2)"),
  {-1, []} = elixir:eval("+(1, -(2))"),
  {-1, []} = elixir:eval("+(=(1, 1), -2 = -2)"),
  {3, [{a,1},{b,2}]} = elixir:eval("+(a = 1, b = 2)").

operators_precedence_test() ->
  {5, []} = elixir:eval("abs -10 + 5"),
  {15, []} = elixir:eval("abs(-10) + 5").

operators_variables_precedence_test() ->
  {30, _} = elixir:eval("a = 10\nb= 20\na+b"),
  % {30, _} = elixir:eval("a = 10\nb= 20\na +b"),
  {30, _} = elixir:eval("a = 10\nb= 20\na + b").

operators_variables_precedence_on_namespaces_test() ->
  F = fun() ->
    elixir:eval("ns Foo; def l: [], do: 1; ns Bar; def l: [x], do: 1;"),
    {3,[]} = elixir:eval("1 + Foo.l + 1"),
    {3,[]} = elixir:eval("1 + Foo.l+1"),
    {2,[]} = elixir:eval("1 + Bar.l +1")
  end,
  test_helper:run_and_remove(F, ['::Foo', '::Bar']).