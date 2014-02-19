-module(string_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

eval(Content) ->
  { Value, Binding, _, _ } = elixir:eval(Content, []),
  { Value, Binding }.

extract_interpolations(String) ->
  element(2, elixir_interpolation:extract(1,
    #elixir_tokenizer{file = <<"nofile">>}, true, String ++ [$"], $")).

% Interpolations

extract_interpolations_without_interpolation_test() ->
  [<<"foo">>] = extract_interpolations("foo").

extract_interpolations_with_escaped_interpolation_test() ->
  [<<"f#{o}o">>] = extract_interpolations("f\\#{o}o").

extract_interpolations_with_interpolation_test() ->
  [<<"f">>,
   {1,[{atom,1,o}]},
   <<"o">>] = extract_interpolations("f#{:o}o").

extract_interpolations_with_two_interpolations_test() ->
  [<<"f">>,
   {1,[{atom,1,o}]},{1,[{atom,1,o}]},
   <<"o">>] = extract_interpolations("f#{:o}#{:o}o").

extract_interpolations_with_only_two_interpolations_test() ->
  [{1,[{atom,1,o}]},
   {1,[{atom,1,o}]}] = extract_interpolations("#{:o}#{:o}").

extract_interpolations_with_tuple_inside_interpolation_test() ->
  [<<"f">>,
   {1,[{'{',1},{number,1,1},{'}',1}]},
   <<"o">>] = extract_interpolations("f#{{1}}o").

extract_interpolations_with_many_expressions_inside_interpolation_test() ->
  [<<"f">>,
   {1,[{number,1,1},{eol,1,newline},{number,2,2}]},
    <<"o">>] = extract_interpolations("f#{1\n2}o").

extract_interpolations_with_right_curly_inside_string_inside_interpolation_test() ->
  [<<"f">>,
   {1,[{bin_string,1,[<<"f}o">>]}]},
   <<"o">>] = extract_interpolations("f#{\"f}o\"}o").

extract_interpolations_with_left_curly_inside_string_inside_interpolation_test() ->
  [<<"f">>,
   {1,[{bin_string,1,[<<"f{o">>]}]},
   <<"o">>] = extract_interpolations("f#{\"f{o\"}o").

extract_interpolations_with_escaped_quote_inside_string_inside_interpolation_test() ->
  [<<"f">>,
   {1,[{bin_string,1,[<<"f\"o">>]}]},
   <<"o">>] = extract_interpolations("f#{\"f\\\"o\"}o").

extract_interpolations_with_less_than_operation_inside_interpolation_test() ->
  [<<"f">>,
   {1,[{number,1,1},{comp_op,1,'<'},{number,1,2}]},
   <<"o">>] = extract_interpolations("f#{1<2}o").

extract_interpolations_with_invalid_expression_inside_interpolation_test() ->
  {1,"invalid token: ",":1}o\""} = extract_interpolations("f#{:1}o").

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

bin_string_with_bell_character_test() ->
  {<<"f\ao">>, _} = eval("\"f\ao\"").

bin_string_with_interpolation_test() ->
  {<<"foo">>, _} = eval("\"f#{\"o\"}o\"").

bin_string_with_another_string_inside_string_inside_interpolation_test() ->
  {<<"fbaro">>, _} = eval("\"f#{\"b#{\"a\"}r\"}o\"").

bin_string_with_another_string_with_curly_inside_interpolation_test() ->
  {<<"fb}ro">>, _} = eval("\"f#{\"b}r\"}o\"").

bin_string_with_atom_with_separator_inside_interpolation_test() ->
  {<<"f}o">>, _} = eval("\"f#{\"}\"}o\"").

bin_string_with_lower_case_hex_interpolation_test() ->
  {<<"jklmno">>, _} = eval("\"\\x6a\\x6b\\x6c\\x6d\\x6e\\x6f\"").

bin_string_with_upper_case_hex_interpolation_test() ->
  {<<"jklmno">>, _} = eval("\"\\x6A\\x6B\\x6C\\x6D\\x6E\\x6F\"").

bin_string_without_interpolation_and_escaped_test() ->
  {<<"f#o">>, _} = eval("\"f\\#o\"").

bin_string_with_escaped_interpolation_test() ->
  {<<"f#{'o}o">>, _} = eval("\"f\\#{'o}o\"").

bin_string_with_the_end_of_line_slash_test() ->
  {<<"fo">>, _} = eval("\"f\\\no\""),
  {<<"fo">>, _} = eval("\"f\\\r\no\"").

invalid_string_interpolation_test() ->
  ?assertError({'Elixir.TokenMissingError', _, _, _, _}, eval("\"f#{some\"")),
  ?assertError({'Elixir.TokenMissingError', _, _, _, _}, eval("\"f#{1+")).

unterminated_string_interpolation_test() ->
  ?assertError({'Elixir.TokenMissingError', _, _, _, _}, eval("\"foo")).

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

list_string_with_bell_character_test() ->
  {"f\ao", _} = eval("'f\ao'").

list_string_with_interpolation_test() ->
  {"foo", _} = eval("'f#{\"o\"}o'").

list_string_with_another_string_with_curly_inside_interpolation_test() ->
  {"fb}ro", _} = eval("'f#{\"b}r\"}o'").

list_string_with_atom_with_separator_inside_interpolation_test() ->
  {"f}o", _} = eval("'f#{\"}\"}o'").

list_string_with_lower_case_hex_interpolation_test() ->
  {"JKLMNO", _} = eval("'\\x4a\\x4b\\x4c\\x4d\\x4e\\x4f'").

list_string_with_upper_case_hex_interpolation_test() ->
  {"JKLMNO", _} = eval("'\\x4A\\x4B\\x4C\\x4D\\x4E\\x4F'").

list_string_without_interpolation_and_escaped_test() ->
  {"f#o", _} = eval("'f\\#o'").

list_string_with_escaped_interpolation_test() ->
  {"f#{\"o}o", _} = eval("'f\\#{\"o}o'").

list_string_with_the_end_of_line_slash_test() ->
  {"fo", _} = eval("'f\\\no'"),
  {"fo", _} = eval("'f\\\r\no'").

char_test() ->
  {99,[]} = eval("?1 + ?2"),
  {10,[]} = eval("?\\n"),
  {40,[]} = eval("?\\(").

%% Binaries

bitstr_with_integer_test() ->
  {<<"fdo">>, _} = eval("<< \"f\", 50+50, \"o\" >>").
