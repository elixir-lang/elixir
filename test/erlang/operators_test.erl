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

and_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
    {true, []} = elixir:eval("true and true"),
    {false, []} = elixir:eval("true and false"),
    {false, []} = elixir:eval("false and true"),
    {false, []} = elixir:eval("false and false"),
    {true, []} = elixir:eval("Bar.foo and Bar.foo"),
    {false, []} = elixir:eval("Bar.foo and Bar.bar"),
    {true, []} = elixir:eval("Bar.foo and Bar.baz 1"),
    {false, []} = elixir:eval("Bar.foo and Bar.baz 2"),
    {true, []} = elixir:eval("1 == 1 and 2 < 3"),
    {true, []} = elixir:eval("false and false or true"),
    ?assertError(badarg, elixir:eval("1 and 2"))
  end,
  test_helper:run_and_remove(F, ['Bar']).

or_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
    {true, []} = elixir:eval("true or true"),
    {true, []} = elixir:eval("true or false"),
    {true, []} = elixir:eval("false or true"),
    {false, []} = elixir:eval("false or false"),
    {true, []} = elixir:eval("Bar.foo or Bar.foo"),
    {true, []} = elixir:eval("Bar.foo or Bar.bar"),
    {false, []} = elixir:eval("Bar.bar or Bar.bar"),
    {true, []} = elixir:eval("Bar.bar or Bar.baz 1"),
    {false, []} = elixir:eval("Bar.bar or Bar.baz 2"),
    ?assertError(badarg, elixir:eval("1 or 2"))
  end,
  test_helper:run_and_remove(F, ['Bar']).

andalso_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
    {true, []} = elixir:eval("true andalso true"),
    {false, []} = elixir:eval("true andalso false"),
    {false, []} = elixir:eval("false andalso true"),
    {false, []} = elixir:eval("false andalso false"),
    {true, []} = elixir:eval("Bar.foo andalso Bar.foo"),
    {false, []} = elixir:eval("Bar.foo andalso Bar.bar"),
    {true, []} = elixir:eval("Bar.foo andalso Bar.baz 1"),
    {false, []} = elixir:eval("Bar.foo andalso Bar.baz 2"),
    {true, []} = elixir:eval("false andalso false orelse true"),
    {3, []} = elixir:eval("Bar.foo andalso 1 + 2"),
    {false, []} = elixir:eval("Bar.bar andalso Erlang.error('bad)"),
    ?assertError({badarg, 1}, elixir:eval("1 andalso 2"))
  end,
  test_helper:run_and_remove(F, ['Bar']).

orelse_test() ->
  F = fun() ->
    elixir:eval("module Bar\ndef foo; true; end\ndef bar; false; end\ndef baz(x); x==1; end\nend"),
    {true, []} = elixir:eval("true orelse true"),
    {true, []} = elixir:eval("true orelse false"),
    {true, []} = elixir:eval("false orelse true"),
    {false, []} = elixir:eval("false orelse false"),
    {true, []} = elixir:eval("Bar.foo orelse Bar.foo"),
    {true, []} = elixir:eval("Bar.foo orelse Bar.bar"),
    {false, []} = elixir:eval("Bar.bar orelse Bar.bar"),
    {true, []} = elixir:eval("Bar.bar orelse Bar.baz 1"),
    {false, []} = elixir:eval("Bar.bar orelse Bar.baz 2"),
    {3, []} = elixir:eval("Bar.bar orelse 1 + 2"),
    {true, []} = elixir:eval("Bar.foo orelse Erlang.error('bad)"),
    ?assertError({badarg, 1}, elixir:eval("1 orelse 2"))
  end,
  test_helper:run_and_remove(F, ['Bar']).

not_test() ->
  {false, []} = elixir:eval("not true"),
  {true, []} = elixir:eval("not false"),
  ?assertError(badarg, elixir:eval("not 1")).