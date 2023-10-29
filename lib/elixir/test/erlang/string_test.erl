-module(string_test).
-include("../../src/elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  Quoted = elixir:'string_to_quoted!'(Content, 1, 1, <<"nofile">>, []),
  {Value, Binding, _} = elixir:eval_forms(Quoted, [], elixir:env_for_eval([])),
  {Value, Binding}.

extract_interpolations(String) ->
  case elixir_interpolation:extract(1, 1, #elixir_tokenizer{}, true, String ++ [$"], $") of
    {error, Error} ->
      Error;
    {_, _, Parts, _, _} ->
      Parts
   end.

% Interpolations

extract_interpolations_without_interpolation_test() ->
  ["foo"] = extract_interpolations("foo").

extract_interpolations_with_escaped_interpolation_test() ->
  ["f\\#{o}o"] = extract_interpolations("f\\#{o}o"),
  {1, 10, ["f\\#{o}o"], [], _} =
    elixir_interpolation:extract(1, 2, #elixir_tokenizer{}, true, "f\\#{o}o\"", $").

extract_interpolations_with_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {1, 6, nil}, [{atom, {1, 4, _}, o}]},
   "o"] = extract_interpolations("f#{:o}o").

extract_interpolations_with_two_interpolations_test() ->
  ["f",
   {{1, 2, nil}, {1, 6, nil}, [{atom, {1, 4, _}, o}]},
   {{1, 7, nil}, {1, 11, nil}, [{atom, {1, 9, _}, o}]},
   "o"] = extract_interpolations("f#{:o}#{:o}o").

extract_interpolations_with_only_two_interpolations_test() ->
  [{{1, 1, nil}, {1, 5, nil}, [{atom, {1, 3, _}, o}]},
   {{1, 6, nil}, {1, 10, nil}, [{atom, {1, 8, _}, o}]}] = extract_interpolations("#{:o}#{:o}").

extract_interpolations_with_tuple_inside_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {1, 7, nil}, [{'{', {1, 4, nil}}, {int, {1, 5, 1}, "1"}, {'}', {1, 6, nil}}]},
   "o"] = extract_interpolations("f#{{1}}o").

extract_interpolations_with_many_expressions_inside_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {2, 2, nil}, [{int, {1, 4, 1}, "1"}, {eol, {1, 5, 1}}, {int, {2, 1, 2}, "2"}]},
    "o"] = extract_interpolations("f#{1\n2}o").

extract_interpolations_with_right_curly_inside_string_inside_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {1, 9, nil}, [{bin_string, {1, 4, nil}, [<<"f}o">>]}]},
   "o"] = extract_interpolations("f#{\"f}o\"}o").

extract_interpolations_with_left_curly_inside_string_inside_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {1, 9, nil}, [{bin_string, {1, 4, nil}, [<<"f{o">>]}]},
   "o"] = extract_interpolations("f#{\"f{o\"}o").

extract_interpolations_with_escaped_quote_inside_string_inside_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {1, 10, nil}, [{bin_string, {1, 4, nil}, [<<"f\"o">>]}]},
   "o"] = extract_interpolations("f#{\"f\\\"o\"}o").

extract_interpolations_with_less_than_operation_inside_interpolation_test() ->
  ["f",
   {{1, 2, nil}, {1, 7, nil}, [{int, {1, 4, 1}, "1"}, {rel_op, {1, 5, nil}, '<'}, {int, {1, 6, 2}, "2"}]},
   "o"] = extract_interpolations("f#{1<2}o").

extract_interpolations_with_an_escaped_character_test() ->
  ["f",
   {{1, 2, nil}, {1, 16, nil}, [{char, {1, 4, "?\\a"}, 7}, {rel_op, {1, 8, nil}, '>'}, {char, {1, 10, "?\\a"}, 7}]}
   ] = extract_interpolations("f#{?\\a > ?\\a   }").

extract_interpolations_with_invalid_expression_inside_interpolation_test() ->
  {[{line, 1}, {column, 4}], "unexpected token: ", _} = extract_interpolations("f#{:1}o").

%% Bin strings

empty_test() ->
  {<<"">>, _} = eval("\"\"").

string_with_double_quotes_test() ->
  {<<"f\"o\"o">>, _} = eval("\"f\\\"o\\\"o\"").

string_with_newline_test() ->
  {<<"f\no">>, _} = eval("\"f\no\"").

string_with_slash_test() ->
  {<<"f\\o">>, _} = eval("\"f\\\\o\"").

string_with_bell_character_test() ->
  {<<"f\ao">>, _} = eval("\"f\ao\"").

string_with_interpolation_test() ->
  {<<"foo">>, _} = eval("\"f#{\"o\"}o\"").

string_with_another_string_inside_string_inside_interpolation_test() ->
  {<<"fbaro">>, _} = eval("\"f#{\"b#{\"a\"}r\"}o\"").

string_with_another_string_with_curly_inside_interpolation_test() ->
  {<<"fb}ro">>, _} = eval("\"f#{\"b}r\"}o\"").

string_with_atom_with_separator_inside_interpolation_test() ->
  {<<"f}o">>, _} = eval("\"f#{\"}\"}o\"").

string_with_lower_case_hex_interpolation_test() ->
  {<<"jklmno">>, _} = eval("\"\\x6a\\x6b\\x6c\\x6d\\x6e\\x6f\"").

string_with_upper_case_hex_interpolation_test() ->
  {<<"jklmno">>, _} = eval("\"\\x6A\\x6B\\x6C\\x6D\\x6E\\x6F\"").

string_without_interpolation_and_escaped_test() ->
  {<<"f#o">>, _} = eval("\"f\\#o\"").

string_with_escaped_interpolation_test() ->
  {<<"f#{'o}o">>, _} = eval("\"f\\#{'o}o\"").

string_with_the_end_of_line_slash_test() ->
  {<<"fo">>, _} = eval("\"f\\\no\""),
  {<<"fo">>, _} = eval("\"f\\\r\no\"").

invalid_string_interpolation_test() ->
  ?assertError(#{'__struct__' := 'Elixir.TokenMissingError'}, eval("\"f#{some\"")),
  ?assertError(#{'__struct__' := 'Elixir.TokenMissingError'}, eval("\"f#{1+")).

unterminated_string_interpolation_test() ->
  ?assertError(#{'__struct__' := 'Elixir.TokenMissingError'}, eval("\"foo")).

char_test() ->
  {99, []} = eval("?1 + ?2"),
  {10, []} = eval("?\\n"),
  {40, []} = eval("?(").
