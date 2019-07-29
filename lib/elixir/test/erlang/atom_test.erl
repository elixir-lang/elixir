-module(atom_test).
-export([kv/1]).
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  {Value, Binding, _, _} =
    elixir:eval_forms(elixir:'string_to_quoted!'(Content, 1, <<"nofile">>, []), [], []),
  {Value, Binding}.

kv([{Key, nil}]) -> Key.

atom_with_punctuation_test() ->
  {foo@bar, []} = eval(":foo@bar"),
  {'a?', []} = eval(":a?"),
  {'a!', []} = eval(":a!"),
  {'||', []} = eval(":||"),
  {'...', []} = eval(":...").

atom_quoted_call_test() ->
  {3, []} = eval("Kernel.'+'(1, 2)").

kv_with_quotes_test() ->
  {'foo bar', []} = eval(":atom_test.kv(\"foo bar\": nil)").

kv_with_interpolation_test() ->
  {'foo', []} = eval(":atom_test.kv(\"#{\"foo\"}\": nil)"),
  {'foo', []} = eval(":atom_test.kv(\"#{\"fo\"}o\": nil)"),
  {'foo', _} = eval("a = \"f\"; :atom_test.kv(\"#{a}#{\"o\"}o\": nil)").

quoted_atom_test() ->
  {'+', []} = eval(":\"+\""),
  {'+', []} = eval(":'+'"),
  {'foo bar', []} = eval(":\"foo bar\"").

atom_with_interpolation_test() ->
  {foo, []} = eval(":\"f#{\"o\"}o\""),
  {foo, _}  = eval("a=\"foo\"; :\"#{a}\""),
  {foo, _}  = eval("a=\"oo\"; :\"f#{a}\""),
  {foo, _}  = eval("a=\"fo\"; :\"#{a}o\""),
  {fof, _}  = eval("a=\"f\"; :\"#{a}o#{a}\"").

quoted_atom_chars_are_escaped_test() ->
  {'"', []} = eval(":\"\\\"\"").
