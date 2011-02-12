-module(operator_test).
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

