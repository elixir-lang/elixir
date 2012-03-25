-module(string_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  { Value, Binding, _ } = elixir:eval(Content, []),
  { Value, Binding }.

extract_interpolations(String) ->
  element(2, elixir_interpolation:extract(1, true, String ++ [$"], $")).

% Interpolations

extract_interpolations_without_interpolation_test() ->
  ["foo"] = extract_interpolations("foo").

extract_interpolations_with_escaped_interpolation_test() ->
  ["f#{o}o"] = extract_interpolations("f\\#{o}o").

extract_interpolations_with_interpolation_test() ->
  ["f", {'|',1,[{{'.',1,['::String::Chars',to_binary]},1,[o]}, binary]}, "o"] = extract_interpolations("f#{:o}o").

extract_interpolations_with_two_interpolations_test() ->
  ["f", {'|',1,[{{'.',1,['::String::Chars',to_binary]},1,[o]}, binary]}, {'|',1,[{{'.',1,['::String::Chars',to_binary]},1,[o]}, binary]}, "o"] = extract_interpolations("f#{:o}#{:o}o").

extract_interpolations_with_only_two_interpolations_test() ->
  [{'|',1,[{{'.',1,['::String::Chars',to_binary]},1,[o]}, binary]}, {'|',1,[{{'.',1,['::String::Chars',to_binary]},1,[o]}, binary]}] = extract_interpolations("#{:o}#{:o}").

extract_interpolations_with_tuple_inside_interpolation_test() ->
  ["f", {'|',1,[{{'.',1,['::String::Chars',to_binary]},1,[{'{}',1,[1]}]}, binary]}, "o"] = extract_interpolations("f#{{1}}o").

extract_interpolations_with_many_expressions_inside_interpolation_test() ->
  ["f", {'|',2,[{{'.',2,['::String::Chars',to_binary]},2,[{'__block__',2,[1,2]}]}, binary]}, "o"] = extract_interpolations("f#{1\n2}o").

extract_interpolations_with_string_inside_interpolation_test() ->
  ["f", <<"foo">>, "o"] = extract_interpolations("f#{\"foo\"}o").

extract_interpolations_with_right_curly_inside_string_inside_interpolation_test() ->
  ["f", <<"f}o">>, "o"] = extract_interpolations("f#{\"f}o\"}o").

extract_interpolations_with_invalid_expression_inside_interpolation_test() ->
  ?assertThrow({interpolation_error, { 1, "invalid token: ", ":1" } }, extract_interpolations("f#{:1}o")).

%% Bin strings

empty_bin_string_test() ->
  {<<"">>, _} = eval("\"\"").

simple_bin_string_test() ->
  {<<"foo">>, _} = eval("\"foo\"").

bin_string_with_double_quotes_test() ->
  {<<"f\"o\"o">>, _} = eval("\"f\\\"o\\\"o\"").

bin_string_with_newline_test() ->
  {<<"f\no">>, _} = eval("\"f\no\"").

bin_string_with_slash_test() ->
  {<<"f\\o">>, _} = eval("\"f\\\\o\"").

bin_string_with_interpolation_test() ->
  {<<"foo">>, _} = eval("\"f#{\"o\"}o\"").

bin_string_with_another_string_inside_string_inside_interpolation_test() ->
  {<<"fbaro">>, _} = eval("\"f#{\"b#{\"a\"}r\"}o\"").

bin_string_with_another_string_with_curly_inside_interpolation_test() ->
  {<<"fb}ro">>, _} = eval("\"f#{\"b}r\"}o\"").

bin_string_with_atom_with_separator_inside_interpolation_test() ->
  {<<"f}o">>, _} = eval("\"f#{\"}\"}o\"").

bin_string_without_interpolation_and_escaped_test() ->
  {<<"f#o">>, _} = eval("\"f\\#o\"").

bin_string_with_escaped_interpolation_test() ->
  {<<"f#{'o}o">>, _} = eval("\"f\\#{'o}o\"").

invalid_string_interpolation_test() ->
  ?assertError({'::SyntaxError', _, _, _, _}, eval("\"f#{{}o\"")).

%% List strings

empty_list_string_test() ->
  {[], _} = eval("\'\'").

simple_list_string_test() ->
  {"foo", _} = eval("'foo'").

list_string_with_double_quotes_test() ->
  {"f'o'o", _} = eval("'f\\'o\\'o'").

list_string_with_newline_test() ->
  {"f\no", _} = eval("'f\no'").

list_string_with_slash_test() ->
  {"f\\o", _} = eval("'f\\\\o'").

list_string_with_interpolation_test() ->
  {"foo", _} = eval("'f#{\"o\"}o'").

list_string_with_another_string_with_curly_inside_interpolation_test() ->
  {"fb}ro", _} = eval("'f#{\"b}r\"}o'").

list_string_with_atom_with_separator_inside_interpolation_test() ->
  {"f}o", _} = eval("'f#{\"}\"}o'").

list_string_without_interpolation_and_escaped_test() ->
  {"f#o", _} = eval("'f\\#o'").

list_string_with_escaped_interpolation_test() ->
  {"f#{\"o}o", _} = eval("'f\\#{\"o}o'").

char_test() ->
  {99,[]} = eval("?1 + ?2"),
  {10,[]} = eval("?\\n"),
  {40,[]} = eval("?\\(").

%% Binaries

bitstr_with_integer_test() ->
  {<<"fdo">>, _} = eval("<< \"f\", 50+50, \"o\" >>").