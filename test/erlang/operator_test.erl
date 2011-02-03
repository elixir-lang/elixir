-module(operator_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Booleans

booleans_test() ->
  {true, []} = elixir:eval("true"),
  {false, []} = elixir:eval("false").

booleans_are_atoms_test() ->
  {"'true", []} = elixir:eval("true.inspect.to_char_list"),
  {"'false", []} = elixir:eval("false.inspect.to_char_list").

if_test() ->
  {1, []} = elixir:eval("if true; 1; end"),
  {[], []} = elixir:eval("if false; 1; end"),
  {2, []} = elixir:eval("if false; 1; else 2; end"),
  {2, []} = elixir:eval("if false; 1; else; 2; end"),
  {3, []} = elixir:eval("if false; 1; elsif true; 3; else; 2; end").
