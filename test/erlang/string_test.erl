-module(string_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

extract_interpolations(String) ->
  element(2, elixir_interpolation:extract(1, true, String ++ [$"], $")).

% Interpolations

extract_interpolations_without_interpolation_test() ->
  ["foo"] = extract_interpolations("foo").

extract_interpolations_with_escaped_interpolation_test() ->
  ["f#{o}o"] = extract_interpolations("f\\#{o}o").

extract_interpolations_with_interpolation_test() ->
  ["f", o, "o"] = extract_interpolations("f#{:o}o").

extract_interpolations_with_two_interpolations_test() ->
  ["f", o, o, "o"] = extract_interpolations("f#{:o}#{:o}o").

extract_interpolations_with_only_two_interpolations_test() ->
  [o, o] = extract_interpolations("#{:o}#{:o}").

extract_interpolations_with_tuple_inside_interpolation_test() ->
  ["f", {'{}',1,[1]}, "o"] = extract_interpolations("f#{{1}}o").

extract_interpolations_with_many_expressions_inside_interpolation_test() ->
  ["f", {block,2,[1,2]}, "o"] = extract_interpolations("f#{1\n2}o").

extract_interpolations_with_string_inside_interpolation_test() ->
  ["f", <<"foo">>, "o"] = extract_interpolations("f#{\"foo\"}o").

extract_interpolations_with_right_curly_inside_string_inside_interpolation_test() ->
  ["f", <<"f}o">>, "o"] = extract_interpolations("f#{\"f}o\"}o").

% extract_interpolations_with_right_curly_inside_regexp_inside_interpolation_test() ->
%   ["f"}, {i, "#r\"f}o\""}, "o"}] = extract_interpolations("f#{#r\"f}o\"}o").

extract_interpolations_with_invalid_expression_inside_interpolation_test() ->
  ?assertError({interpolation_error, { 1, "invalid token", ":1" } }, extract_interpolations("f#{:1}o")).

%% Bin strings

simple_bin_string_test() ->
  {<<"foo">>, _} = elixir:eval("\"foo\"").

bin_string_with_double_quotes_test() ->
  {<<"f\"o\"o">>, _} = elixir:eval("\"f\\\"o\\\"o\"").

bin_string_with_newline_test() ->
  {<<"f\no">>, _} = elixir:eval("\"f\no\"").

bin_string_with_slash_test() ->
  {<<"f\\o">>, _} = elixir:eval("\"f\\\\o\"").

bin_string_with_interpolation_test() ->
  {<<"foo">>, _} = elixir:eval("\"f#{\"o\"}o\"").

bin_string_with_another_string_with_curly_inside_interpolation_test() ->
  {<<"fb}ro">>, _} = elixir:eval("\"f#{\"b}r\"}o\"").

bin_string_with_atom_with_separator_inside_interpolation_test() ->
  {<<"f}o">>, _} = elixir:eval("\"f#{\"}\"}o\"").

bin_string_without_interpolation_and_escaped_test() ->
  {<<"f#o">>, _} = elixir:eval("\"f\\#o\"").

bin_string_with_escaped_interpolation_test() ->
  {<<"f#{'o}o">>, _} = elixir:eval("\"f\\#{'o}o\"").

invalid_string_interpolation_test() ->
  ?assertError({badsyntax, {1, <<"nofile">>, _, _}}, elixir:eval("\"f#{{}o\"")).

%% List strings

simple_list_string_test() ->
  {"foo", _} = elixir:eval("'foo'").

list_string_with_double_quotes_test() ->
  {"f'o'o", _} = elixir:eval("'f\\'o\\'o'").

list_string_with_newline_test() ->
  {"f\no", _} = elixir:eval("'f\no'").

list_string_with_slash_test() ->
  {"f\\o", _} = elixir:eval("'f\\\\o'").

list_string_with_interpolation_test() ->
  {"foo", _} = elixir:eval("'f#{\"o\"}o'").

list_string_with_another_string_with_curly_inside_interpolation_test() ->
  {"fb}ro", _} = elixir:eval("'f#{\"b}r\"}o'").

list_string_with_atom_with_separator_inside_interpolation_test() ->
  {"f}o", _} = elixir:eval("'f#{\"}\"}o'").

list_string_without_interpolation_and_escaped_test() ->
  {"f#o", _} = elixir:eval("'f\\#o'").

list_string_with_escaped_interpolation_test() ->
  {"f#{\"o}o", _} = elixir:eval("'f\\#{\"o}o'").

% char_test() ->
%   {99,[]} = elixir:eval("$1 + $2"),
%   {10,[]} = elixir:eval("$\\n"),
%   {40,[]} = elixir:eval("$\\(").
%     
% bad_char_test() ->
%   ?assertError({badsyntax, _}, elixir:eval("$foo")).
% 
% implicit_string_concatenation_test() ->
%   {<<"foobar">>, []} = elixir:eval("\"foo\" \"bar\""),
%   {<<"foobar">>, []} = elixir:eval("\"foo\" \\\n \"bar\""),
%   {<<"foobarbaz">>, []} = elixir:eval("\"foo\" \\\n \"b#{'a}r\"\\\n\"baz\"").
% 
% %% Sigils and Char lists
% 
% string_sigils_test() ->
%   {<<"f#{o}o">>, []} = elixir:eval("~q(f#{o}o)"),
%   {<<"bar">>, []} = elixir:eval("~Q(b#{'a}r)"),
%   {<<"b)r">>, []} = elixir:eval("~Q(b\\)r)").
% 
% implicit_string_sigils_concatenation_test() ->
%   {<<"f#{o}obar">>, []} = elixir:eval("~q(f#{o}o) ~Q(b#{'a}r)"),
%   {<<"f#{o}obar">>, []} = elixir:eval("~q(f#{o}o) \\\n ~Q(b#{'a}r)").
% 
% char_list_test() ->
%   {"foo", []}    = elixir:eval("$\"foo\""),
%   {"foo", []}    = elixir:eval("$(foo)"),
%   {"f#{o}o", []} = elixir:eval("$(f\\#{o}o)"),
%   {"foo", []}    = elixir:eval("$[f#{'o}o]"),
%   {"f#{o}o", []} = elixir:eval("~l(f#{o}o)"),
%   {"bar", []}    = elixir:eval("~L(b#{'a}r)"),
%   {"b)r", []}    = elixir:eval("~L(b\\)r)").
% 
% %% Inspect in other objects
% 
% list_inspect_test() ->
%   {<<"[1,2,3]">>, []} = elixir:eval("[1,2,3].inspect"),
%   {<<"[102,111,111]">>, []} = elixir:eval("\"foo\".to_char_list.inspect"),
%   {<<"[1,2,3]">>, []} = elixir:eval("[1,2,3].to_s"),
%   {<<"[102,111,111]">>, []} = elixir:eval("\"foo\".to_char_list.to_s").
% 
% tuple_inspect_test() ->
%   {<<"{'badmatch,true}">>, []} = elixir:eval("{'badmatch,true}.inspect").

%% Binaries

bitstr_with_integer_test() ->
  {<<"fdo">>, _} = elixir:eval("bitstr \"f\", 50+50, \"o\"").