-module(operators_test).
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  {Value, Binding, _, _} =
    elixir:eval_forms(elixir:'string_to_quoted!'(Content, 1, <<"nofile">>, []), [], []),
  {Value, Binding}.

separator_test() ->
  {334, []} = eval("3_34"),
  {600, []} = eval("2_00+45_5-5_5").

integer_sum_test() ->
  {3, []} = eval("1+2"),
  {6, []} = eval("1+2+3"),
  {6, []} = eval("1+2 +3"),
  {6, []} = eval("1 + 2 + 3").

integer_sum_minus_test() ->
  {-4, []} = eval("1-2-3"),
  {0, []}  = eval("1+2-3"),
  {0, []}  = eval("1 + 2 - 3").

integer_mult_test() ->
  {6, []} = eval("1*2*3"),
  {6, []} = eval("1 * 2 * 3").

integer_div_test() ->
  {0.5, []} = eval("1 / 2"),
  {2.0, []} = eval("4 / 2").

integer_div_rem_test() ->
  {2, []} = eval("div 5, 2"),
  {1, []} = eval("rem 5, 2").

integer_mult_div_test() ->
  {1.0, []} = eval("2*1/2"),
  {6.0, []} = eval("3 * 4 / 2").

integer_without_parens_test() ->
  {17, []}  = eval("3 * 5 + 2"),
  {17, []}  = eval("2 + 3 * 5"),
  {6.0, []} = eval("4 / 4 + 5").

integer_with_parens_test() ->
  {21, []}   = eval("3 * (5 + 2)"),
  {21, []}   = eval("3 * (((5 + (2))))"),
  {25, []}   = eval("(2 + 3) * 5"),
  {0.25, []} = eval("4 / (11 + 5)").

integer_with_unary_test() ->
  {2, []}    = eval("- 1 * - 2").

integer_eol_test() ->
  {3, []} = eval("1 +\n2"),
  {2, []} = eval("1 *\n2"),
  {8, []} = eval("1 + 2\n3 + 5"),
  {8, []} = eval("1 + 2\n\n\n3 + 5"),
  {8, []} = eval("1 + 2;\n\n3 + 5"),
  {8, []} = eval("1 + (\n2\n) + 3 + 2"),
  {8, []} = eval("1 + (\n\n  2\n\n) + 3 + 2"),
  {3, []} = eval(";1 + 2"),
  ?assertError(#{'__struct__' := 'Elixir.SyntaxError'}, eval("1 + 2;\n;\n3 + 5")).

float_with_parens_and_unary_test() ->
  {-21.0, []} = eval("-3.0 * (5 + 2)"),
  {25.0, []}  = eval("(2 + 3.0) * 5"),
  {0.25, []}  = eval("4 / (11.0 + 5)").

operators_precedence_test() ->
  {2, _}   = eval("max -1, 2"),
  {5, []}  = eval("abs -10 + 5"),
  {15, []} = eval("abs(-10) + 5").

operators_variables_precedence_test() ->
  {30, _} = eval("a = 10\nb= 20\na+b"),
  {30, _} = eval("a = 10\nb= 20\na + b").

operators_variables_precedence_on_namespaces_test() ->
  F = fun() ->
    eval("defmodule Foo do; def l, do: 1; end; defmodule Bar do; def l(_x), do: 1; end"),
    {3, []} = eval("1 + Foo.l + 1"),
    {3, []} = eval("1 + Foo.l+1"),
    {2, []} = eval("1 + Bar.l +1")
  end,
  test_helper:run_and_remove(F, ['Elixir.Foo', 'Elixir.Bar']).

add_add_op_test() ->
  {[1, 2, 3, 4], []} = eval("[1, 2] ++ [3, 4]").

minus_minus_op_test() ->
  {[1, 2], []} = eval("[1, 2, 3] -- [3]"),
  {[1, 2, 3], []} = eval("[1, 2, 3] -- [3] -- [3]").
