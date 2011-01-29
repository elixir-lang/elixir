-module(atom_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

atom_inspect_test() ->
  {String,[]} = elixir:eval("'a.inspect"),
  "'a" = test_helper:unpack_string(String).

atom_to_s_test() ->
  {String,[]} = elixir:eval("'a.to_s"),
  "a" = test_helper:unpack_string(String).

quoted_atom_chars_are_escaped_test() ->
  {'"',[]} = elixir:eval("'\"\\\"\"").