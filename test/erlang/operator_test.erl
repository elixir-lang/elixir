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
