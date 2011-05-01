-module(string_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

% Evaluate the Expr returning String internal information.
eval_string(Expr) ->
  { String, Binding } = elixir:eval(Expr),
  { test_helper:unpack_string(String), Binding }.

extract_interpolations(String) ->
  element(1, elixir_interpolation:extract(true, String ++ [$)], $))).

% Interpolations

extract_interpolations_without_interpolation_test() ->
  [{s, "foo"}] = extract_interpolations("foo").

extract_interpolations_with_escaped_interpolation_test() ->
  [{s, "f#{o}o"}] = extract_interpolations("f\\#{o}o").

extract_interpolations_with_interpolation_test() ->
  [{s, "f"}, {i, "o"}, {s, "o"}] = extract_interpolations("f#{o}o").

extract_interpolations_with_two_interpolations_test() ->
  [{s, "f"}, {i, "o"}, {i, "o"}, {s, "o"}] = extract_interpolations("f#{o}#{o}o").

extract_interpolations_with_only_two_interpolations_test() ->
  [{i, "o"}, {i, "o"}] = extract_interpolations("#{o}#{o}").

extract_interpolations_with_tuple_inside_interpolation_test() ->
  [{s, "f"}, {i, "{1}"}, {s, "o"}] = extract_interpolations("f#{{1}}o").

extract_interpolations_with_string_inside_interpolation_test() ->
  [{s, "f"}, {i, "\"foo\""}, {s, "o"}] = extract_interpolations("f#{\"foo\"}o").

extract_interpolations_with_right_curly_inside_string_inside_interpolation_test() ->
  [{s, "f"}, {i, "\"f}o\""}, {s, "o"}] = extract_interpolations("f#{\"f}o\"}o").

extract_interpolations_with_right_curly_inside_regexp_inside_interpolation_test() ->
  [{s, "f"}, {i, "#r\"f}o\""}, {s, "o"}] = extract_interpolations("f#{#r\"f}o\"}o").

%% String

simple_string_test() ->
  {<<"foo">>, _} = eval_string("\"foo\"").

string_with_double_quotes_test() ->
  {<<"f\"o\"o">>, _} = eval_string("\"f\\\"o\\\"o\"").

string_with_newline_test() ->
  {<<"f\no">>, _} = eval_string("\"f\no\"").

string_with_slash_test() ->
  {<<"f\\o">>, _} = eval_string("\"f\\\\o\"").

string_with_interpolation_test() ->
  {<<"foo">>, _} = eval_string("\"f#{'o}o\"").

string_with_another_string_interpolation_test() ->
  {<<"foo">>, _} = eval_string("\"f#{\"o\"}o\"").

string_with_another_string_inside_string_interpolation_test() ->
  {<<"fbaro">>, _} = eval_string("\"f#{\"b#{'a}r\"}o\"").

string_with_another_string_with_curly_inside_interpolation_test() ->
  {<<"fb}ro">>, _} = eval_string("\"f#{\"b}r\"}o\"").

string_with_atom_with_separator_inside_interpolation_test() ->
  {<<"f}o">>, _} = eval_string("\"f#{\"}\"}o\"").

string_without_interpolation_and_escaped_test() ->
  {<<"f#o">>, _} = eval_string("\"f\\#o\"").

string_with_escaped_interpolation_test() ->
  {<<"f#{'o}o">>, _} = eval_string("\"f\\#{'o}o\"").

invalid_string_interpolation_test() ->
  ?assertError({badsyntax, {1, "nofile", _, _}}, elixir:eval("\"f#{{}o\"")).

char_test() ->
  {99,[]} = elixir:eval("$1 + $2"),
  {10,[]} = elixir:eval("$\\n"),
  {40,[]} = elixir:eval("$\\(").
    
bad_char_test() ->
  ?assertError({badsyntax, _}, elixir:eval("$foo")).

implicit_string_concatenation_test() ->
  {<<"foobar">>, []} = eval_string("\"foo\" \"bar\""),
  {<<"foobar">>, []} = eval_string("\"foo\" \\\n \"bar\""),
  {<<"foobarbaz">>, []} = eval_string("\"foo\" \\\n \"b#{'a}r\"\\\n\"baz\"").

%% Methods

string_to_s_returns_self_test() ->
  {<<"elixir">>, []} = eval_string("\"elixir\".to_s").

string_inspect_test() ->
  {<<"\"elixir\"">>, []} = eval_string("\"elixir\".inspect").

string_to_list_test() ->
  {"elixir", []} = elixir:eval("\"elixir\".to_list").

string_length_test() ->
  {6, []} = elixir:eval("\"elixir\".length").

string_add_test() ->
  {<<"elixir">>, []} = eval_string("\"eli\" + \"xir\"").

%% Sigils and Char lists

string_sigils_test() ->
  {<<"f#{o}o">>, []} = eval_string("~q(f#{o}o)"),
  {<<"bar">>, []} = eval_string("~Q(b#{'a}r)"),
  {<<"b)r">>, []} = eval_string("~Q(b\\)r)").

implicit_string_sigils_concatenation_test() ->
  {<<"f#{o}obar">>, []} = eval_string("~q(f#{o}o) ~Q(b#{'a}r)"),
  {<<"f#{o}obar">>, []} = eval_string("~q(f#{o}o) \\\n ~Q(b#{'a}r)").

char_list_test() ->
  {"foo", []}    = elixir:eval("$\"foo\""),
  {"foo", []}    = elixir:eval("$(foo)"),
  {"f#{o}o", []} = elixir:eval("$(f\\#{o}o)"),
  {"foo", []}    = elixir:eval("$[f#{'o}o]"),
  {"f#{o}o", []} = elixir:eval("~l(f#{o}o)"),
  {"bar", []}    = elixir:eval("~L(b#{'a}r)"),
  {"b)r", []}    = elixir:eval("~L(b\\)r)").

%% Inspect in other objects

list_inspect_test() ->
  {<<"[1,2,3]">>, []} = eval_string("[1,2,3].inspect"),
  {<<"[102,111,111]">>, []} = eval_string("\"foo\".to_char_list.inspect"),
  {<<"[1,2,3]">>, []} = eval_string("[1,2,3].to_s"),
  {<<"[102,111,111]">>, []} = eval_string("\"foo\".to_char_list.to_s").

tuple_inspect_test() ->
  {<<"{'badmatch,true}">>, []} = eval_string("{'badmatch,true}.inspect").