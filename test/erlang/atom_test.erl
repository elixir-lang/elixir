-module(atom_test).
-export([kv/1]).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

kv([{Key,nil}]) -> Key.

atom_with_punctuation_test() ->
  {'a?',[]} = elixir:eval(":a?"),
  {'a!',[]} = elixir:eval(":a!"),
  {'||',[]} = elixir:eval(":||").

kv_with_punctuation_test() ->
  {'a?',[]} = elixir:eval("Erlang.atom_test.kv(a?: nil)"),
  {'a!',[]} = elixir:eval("Erlang.atom_test.kv(a!: nil)"),
  {'||',[]} = elixir:eval("Erlang.atom_test.kv(||: nil)").

quoted_atom_test() ->
  {foo,[]} = elixir:eval(":\"foo\"").

atom_with_interpolation_test() ->
  {foo,[]} = elixir:eval(":\"f#{\"o\"}o\"").

quoted_atom_chars_are_escaped_test() ->
  {'"',[]} = elixir:eval(":\"\\\"\"").