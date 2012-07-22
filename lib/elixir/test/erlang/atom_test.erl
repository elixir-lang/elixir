-module(atom_test).
-export([kv/1]).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile({parse_transform, elixir_transform}).

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

kv([{Key,nil}]) -> Key.

atom_with_punctuation_test() ->
  {foo@bar,[]} = eval(":foo@bar"),
  {'a?',[]} = eval(":a?"),
  {'a!',[]} = eval(":a!"),
  {'||',[]} = eval(":||").

atom_quoted_call_test() ->
  {3,[]} = eval("Kernel.'+'(1, 2)").

kv_with_quotes_test() ->
  {'foo bar',[]} = eval("Erlang.atom_test.kv(\"foo bar\": nil)").

quoted_atom_test() ->
  {foo,[]} = eval(":\"foo\""),
  {foo,[]} = eval(":'foo'").

atom_with_interpolation_test() ->
  {foo,[]} = eval(":\"f#{\"o\"}o\"").

quoted_atom_chars_are_escaped_test() ->
  {'"',[]} = eval(":\"\\\"\"").