-module(operators_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Booleans

booleans_test() ->
  {true, []} = elixir:eval("true"),
  {false, []} = elixir:eval("false").

if_test() ->
  {1, []} = elixir:eval("if true; 1; end"),
  {[], []} = elixir:eval("if false; 1; end"),
  {2, []} = elixir:eval("if false; 1; else 2; end"),
  {2, []} = elixir:eval("if false; 1; else; 2; end"),
  {3, []} = elixir:eval("if false; 1; elsif true; 3; else; 2; end"),
  {3, []} = elixir:eval("if false\n 1\n elsif true\n 3\n else\n 2\n end"),
  {3, []} = elixir:eval("if false then 1 elsif true then 3 else 2 end").

unless_test() ->
  {1, []} = elixir:eval("unless false; 1; end"),
  {[], []} = elixir:eval("unless true; 1; end"),
  {2, []} = elixir:eval("unless true; 1; else 2; end"),
  {2, []} = elixir:eval("unless true; 1; else; 2; end"),
  {3, []} = elixir:eval("unless true; 1; elsif true; 3; else; 2; end").

vars_if_test() ->
  F = fun() ->
    {1, [{foo,1}]} = elixir:eval("if foo = 1; true; else; false; end; foo"),
    elixir:eval("module Bar\ndef foo; 1; end\ndef bar(x); if x; foo = 2; else; foo = foo; end; foo; end\nend"),
    {1, []} = elixir:eval("Bar.bar(false)"),
    {2, []} = elixir:eval("Bar.bar(true)")
  end,
  test_helper:run_and_remove(F, ['Bar']).

%% Comparison

equal_test() ->
  {true,[]} = elixir:eval("'a == 'a"),
  {true,[]} = elixir:eval("1 == 1"),
  {true,[]} = elixir:eval("~q(a) == ~q(a)"),
  {true,[]} = elixir:eval("{1,2} == {1,2}"),
  {true,[]} = elixir:eval("[1,2] == [1,2]"),
  {false,[]} = elixir:eval("'a == 'b"),
  {false,[]} = elixir:eval("1 == 2"),
  {false,[]} = elixir:eval("~q(a) == ~q(b)"),
  {false,[]} = elixir:eval("{1,2} == {1,3}"),
  {false,[]} = elixir:eval("[1,2] == [1,3]").

not_equal_test() ->
  {false,[]} = elixir:eval("'a != 'a"),
  {false,[]} = elixir:eval("1 != 1"),
  {false,[]} = elixir:eval("~q(a) != ~q(a)"),
  {false,[]} = elixir:eval("{1,2} != {1,2}"),
  {false,[]} = elixir:eval("[1,2] != [1,2]"),
  {true,[]} = elixir:eval("'a != 'b"),
  {true,[]} = elixir:eval("1 != 2"),
  {true,[]} = elixir:eval("~q(a) != ~q(b)"),
  {true,[]} = elixir:eval("{1,2} != {1,3}"),
  {true,[]} = elixir:eval("[1,2] != [1,3]").

not_exclamation_mark_test() ->
  {false,[]} = elixir:eval("!'a"),
  {false,[]} = elixir:eval("!true"),
  {false,[]} = elixir:eval("!1"),
  {true,[]} = elixir:eval("![]"),
  {true,[]} = elixir:eval("!false").

notnot_exclamation_mark_test() ->
  {true,[]} = elixir:eval("!!'a"),
  {true,[]} = elixir:eval("!!true"),
  {true,[]} = elixir:eval("!!1"),
  {false,[]} = elixir:eval("!![]"),
  {false,[]} = elixir:eval("!!false").

less_greater_test() ->
  {true,[]} = elixir:eval("1 < 2"),
  {true,[]} = elixir:eval("1 < 'a"),
  {false,[]} = elixir:eval("1 < 1.0"),
  {false,[]} = elixir:eval("1 < 1"),
  {true,[]} = elixir:eval("1 <= 1.0"),
  {true,[]} = elixir:eval("1 <= 1"),
  {true,[]} = elixir:eval("1 <= 'a"),
  {false,[]} = elixir:eval("1 > 2"),
  {false,[]} = elixir:eval("1 > 'a"),
  {false,[]} = elixir:eval("1 > 1.0"),
  {false,[]} = elixir:eval("1 > 1"),
  {true,[]} = elixir:eval("1 >= 1.0"),
  {true,[]} = elixir:eval("1 >= 1"),
  {false,[]} = elixir:eval("1 >= 'a").

integer_and_float_test() ->
  {true,[]} = elixir:eval("1 == 1"),
  {false,[]} = elixir:eval("1 != 1"),
  {true,[]} = elixir:eval("1 == 1.0"),
  {false,[]} = elixir:eval("1 != 1.0"),
  {true,[]} = elixir:eval("1 =:= 1"),
  {false,[]} = elixir:eval("1 =!= 1"),
  {false,[]} = elixir:eval("1 =:= 1.0"),
  {true,[]} = elixir:eval("1 =!= 1.0").