-module(tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  tokenize(String, []).

tokenize(String, Opts) ->
  {ok, Result} = elixir_tokenizer:tokenize(String, 1, Opts),
  Result.

tokenize_error(String) ->
  {error, Error, _, _} = elixir_tokenizer:tokenize(String, 1, []),
  Error.

type_test() ->
  [{int, {1, 1, 1}, "1"},
   {type_op, {1, 3, nil}, '::'},
   {int, {1, 6, 3}, "3"}] = tokenize("1 :: 3"),
  [{identifier, {1, 1, nil}, name},
   {'.', {1, 5, nil}},
   {paren_identifier, {1, 6, nil}, '::'},
   {'(', {1, 8, nil}},
   {int, {1, 9, 3}, "3"},
   {')', {1, 10, nil}}] = tokenize("name.::(3)").

arithmetic_test() ->
  [{int, {1, 1, 1}, "1"},
   {dual_op, {1, 3, nil}, '+'},
   {int, {1, 5, 2}, "2"},
   {dual_op, {1, 7, nil}, '+'},
   {int, {1, 9, 3}, "3"}] = tokenize("1 + 2 + 3").

op_kw_test() ->
  [{atom, {1, 1, nil}, foo},
   {dual_op, {1, 5, nil}, '+'},
   {atom, {1, 6, nil}, bar}] = tokenize(":foo+:bar").

scientific_test() ->
  [{float, {1, 1, 0.1}, "1.0e-1"}] = tokenize("1.0e-1"),
  [{float, {1, 1, 0.1}, "1.0E-1"}] = tokenize("1.0E-1"),
  [{float, {1, 1, 1.2345678e-7}, "1_234.567_8e-10"}] = tokenize("1_234.567_8e-10"),
  {1, 1, "invalid float number ", "1.0e309"} = tokenize_error("1.0e309").

hex_bin_octal_test() ->
  [{int, {1, 1, 255}, "0xFF"}] = tokenize("0xFF"),
  [{int, {1, 1, 255}, "0xF_F"}] = tokenize("0xF_F"),
  [{int, {1, 1, 63}, "0o77"}] = tokenize("0o77"),
  [{int, {1, 1, 63}, "0o7_7"}] = tokenize("0o7_7"),
  [{int, {1, 1, 3}, "0b11"}] = tokenize("0b11"),
  [{int, {1, 1, 3}, "0b1_1"}] = tokenize("0b1_1").

unquoted_atom_test() ->
  [{atom, {1, 1, nil}, '+'}] = tokenize(":+"),
  [{atom, {1, 1, nil}, '-'}] = tokenize(":-"),
  [{atom, {1, 1, nil}, '*'}] = tokenize(":*"),
  [{atom, {1, 1, nil}, '/'}] = tokenize(":/"),
  [{atom, {1, 1, nil}, '='}] = tokenize(":="),
  [{atom, {1, 1, nil}, '&&'}] = tokenize(":&&").

quoted_atom_test() ->
  [{atom, {1, 1, nil}, 'foo bar'}] = tokenize(":\"foo bar\"").

oversized_atom_test() ->
  OversizedAtom = string:copies("a", 256),
  {1, 1, "atom length must be less than system limit: ", OversizedAtom} =
    tokenize_error([$: | OversizedAtom]).

op_atom_test() ->
  [{atom, {1, 1, nil}, f0_1}] = tokenize(":f0_1").

kw_test() ->
  [{kw_identifier, {1, 1, nil}, do}] = tokenize("do: "),
  [{kw_identifier, {1, 1, nil}, a@}] = tokenize("a@: "),
  [{kw_identifier, {1, 1, nil}, 'A@'}] = tokenize("A@: "),
  [{kw_identifier, {1, 1, nil}, a@b}] = tokenize("a@b: "),
  [{kw_identifier, {1, 1, nil}, 'A@!'}] = tokenize("A@!: "),
  [{kw_identifier, {1, 1, nil}, 'a@!'}] = tokenize("a@!: "),
  [{kw_identifier, {1, 1, nil}, foo}, {bin_string, {1, 6, nil}, [<<"bar">>]}] = tokenize("foo: \"bar\""),
  [{kw_identifier_unsafe, {1, 1, nil}, [<<"+">>]}, {bin_string, {1, 6, nil}, [<<"bar">>]}] = tokenize("\"+\": \"bar\"").

int_test() ->
  [{int, {1, 1, 123}, "123"}] = tokenize("123"),
  [{int, {1, 1, 123}, "123"}, {';', {1, 4, 0}}] = tokenize("123;"),
  [{eol, {1, 1, 2}}, {int, {3, 1, 123}, "123"}] = tokenize("\n\n123"),
  [{int, {1, 3, 123}, "123"}, {int, {1, 8, 234}, "234"}] = tokenize("  123  234  "),
  [{int, {1, 1, 7}, "007"}] = tokenize("007"),
  [{int, {1, 1, 100000}, "0100000"}] = tokenize("0100000").

float_test() ->
  [{float, {1, 1, 12.3}, "12.3"}] = tokenize("12.3"),
  [{float, {1, 1, 12.3}, "12.3"}, {';', {1, 5, 0}}] = tokenize("12.3;"),
  [{eol, {1, 1, 2}}, {float, {3, 1, 12.3}, "12.3"}] = tokenize("\n\n12.3"),
  [{float, {1, 3, 12.3}, "12.3"}, {float, {1, 9, 23.4}, "23.4"}] = tokenize("  12.3  23.4  "),
  [{float, {1, 1, 12.3}, "00_12.3_00"}] = tokenize("00_12.3_00"),
  OversizedFloat = string:copies("9", 310) ++ ".0",
  {1, 1, "invalid float number ", OversizedFloat} = tokenize_error(OversizedFloat).

identifier_test() ->
  [{identifier, {1, 1, nil}, abc}] = tokenize("abc "),
  [{identifier, {1, 1, nil}, 'abc?'}] = tokenize("abc?"),
  [{identifier, {1, 1, nil}, 'abc!'}] = tokenize("abc!"),
  [{identifier, {1, 1, nil}, 'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier, {1, 1, nil}, 'a0c'}, {'(', {1, 4, nil}}, {')', {1, 5, nil}}] = tokenize("a0c()"),
  [{paren_identifier, {1, 1, nil}, 'a0c!'}, {'(', {1, 5, nil}}, {')', {1, 6, nil}}] = tokenize("a0c!()").

module_macro_test() ->
  [{identifier, {1, 1, nil}, '__MODULE__'}] = tokenize("__MODULE__").

triple_dot_test() ->
  [{identifier, {1, 1, nil}, '...'}] = tokenize("..."),
  [{'.', {1, 1, nil}}, {identifier, {1, 3, nil}, '..'}] = tokenize(". ..").

dot_test() ->
  [{identifier, {1, 1, nil}, foo},
   {'.', {1, 4, nil}},
   {identifier, {1, 5, nil}, bar},
   {'.', {1, 8, nil}},
   {identifier, {1, 9, nil}, baz}] = tokenize("foo.bar.baz").

dot_keyword_test() ->
 [{identifier, {1, 1, nil}, foo},
  {'.', {1, 4, nil}},
  {identifier, {1, 5, nil}, do}] = tokenize("foo.do").

newline_test() ->
  [{identifier, {1, 1, nil}, foo},
   {'.', {2, 1, nil}},
   {identifier, {2, 2, nil}, bar}]  = tokenize("foo\n.bar"),
  [{int, {1, 1, 1}, "1"},
   {two_op, {2, 1, 1}, '++'},
   {int, {2, 3, 2}, "2"}]  = tokenize("1\n++2").

dot_newline_operator_test() ->
  [{identifier, {1, 1, nil}, foo},
   {'.', {1, 4, nil}},
   {identifier, {2, 1, nil}, '+'},
   {int, {2, 2, 1}, "1"}] = tokenize("foo.\n+1"),
  [{identifier, {1, 1, nil}, foo},
   {'.', {1, 4, nil}},
   {identifier, {2, 1, nil}, '+'},
   {int, {2, 2, 1}, "1"}] = tokenize("foo.#bar\n+1").

dot_call_operator_test() ->
  [{identifier, {1, 1, nil}, f},
   {dot_call_op, {1, 2, nil}, '.'},
   {'(', {1, 3, nil}},
   {')', {1, 4, nil}}] = tokenize("f.()").

aliases_test() ->
  [{'alias', {1, 1, nil}, 'Foo'}] = tokenize("Foo"),
  [{'alias', {1, 1, nil}, 'Foo'},
   {'.', {1, 4, nil}},
   {'alias', {1, 5, nil}, 'Bar'},
   {'.', {1, 8, nil}},
   {'alias', {1, 9, nil}, 'Baz'}] = tokenize("Foo.Bar.Baz").

string_test() ->
  [{bin_string, {1, 1, nil}, [<<"foo">>]}] = tokenize("\"foo\""),
  [{bin_string, {1, 1, nil}, [<<"f\"">>]}] = tokenize("\"f\\\"\""),
  [{list_string, {1, 1, nil}, [<<"foo">>]}] = tokenize("'foo'").

heredoc_test() ->
  [{bin_heredoc, {1, 1, nil}, [<<"heredoc\n">>]}] = tokenize("\"\"\"\nheredoc\n\"\"\""),
  [{bin_heredoc, {1, 1, nil}, [<<"heredoc\n">>]}, {';', {3, 5, 0}}] = tokenize("\"\"\"\n heredoc\n \"\"\";").

empty_string_test() ->
  [{bin_string, {1, 1, nil}, [<<>>]}] = tokenize("\"\""),
  [{list_string, {1, 1, nil}, [<<>>]}] = tokenize("''").

addadd_test() ->
  [{identifier, {1, 1, nil}, x},
   {two_op, {1, 3, nil}, '++'},
   {identifier, {1, 6, nil}, y}] = tokenize("x ++ y").

space_test() ->
  [{op_identifier, {1, 1, nil}, foo},
   {dual_op, {1, 5, nil}, '-'},
   {int, {1, 6, 2}, "2"}] = tokenize("foo -2"),
  [{op_identifier, {1, 1, nil}, foo},
   {dual_op, {1, 6, nil}, '-'},
   {int, {1, 7, 2}, "2"}] = tokenize("foo  -2").

chars_test() ->
  [{char, {1, 1, "?a"}, 97}] = tokenize("?a"),
  [{char, {1, 1, "?c"}, 99}] = tokenize("?c"),
  [{char, {1, 1, "?\\0"}, 0}]  = tokenize("?\\0"),
  [{char, {1, 1, "?\\a"}, 7}]  = tokenize("?\\a"),
  [{char, {1, 1, "?\\n"}, 10}] = tokenize("?\\n"),
  [{char, {1, 1, "?\\\\"}, 92}] = tokenize("?\\\\").

interpolation_test() ->
  [{bin_string, {1, 1, nil}, [<<"f">>, {{1, 3, nil},{1, 7, nil}, [{identifier, {1, 5, nil}, oo}]}]},
   {two_op, {1, 10, nil}, '<>'},
   {bin_string, {1, 13, nil}, [<<>>]}] = tokenize("\"f#{oo}\" <> \"\"").

capture_test() ->
  [{capture_op, {1, 1, nil}, '&'},
   {identifier, {1, 2, nil}, '||'},
   {mult_op, {1, 4, nil}, '/'},
   {int, {1, 5, 2}, "2"}] = tokenize("&||/2"),
  [{capture_op, {1, 1, nil}, '&'},
   {identifier, {1, 2, nil}, 'or'},
   {mult_op, {1, 4, nil}, '/'},
   {int, {1, 5, 2}, "2"}] = tokenize("&or/2"),
  [{capture_op, {1, 1, nil}, '&'},
   {unary_op, {1, 2, nil}, 'not'},
   {int, {1, 6, 1}, "1"},
   {',', {1, 7, 0}},
   {int, {1, 9, 2}, "2"}] = tokenize("&not 1, 2").

vc_merge_conflict_test() ->
  {1, 1, "found an unexpected version control marker, please resolve the conflicts: ", "<<<<<<< HEAD"} =
    tokenize_error("<<<<<<< HEAD\n[1, 2, 3]").

sigil_terminator_test() ->
  [{sigil, {1, 1, nil}, 114, [<<"foo">>], [], <<"/">>}] = tokenize("~r/foo/"),
  [{sigil, {1, 1, nil}, 114, [<<"foo">>], [], <<"[">>}] = tokenize("~r[foo]"),
  [{sigil, {1, 1, nil}, 114, [<<"foo">>], [], <<"\"">>}] = tokenize("~r\"foo\""),
  [{sigil, {1, 1, nil}, 114, [<<"foo">>], [], <<"/">>},
   {comp_op, {1, 9, nil}, '=='},
   {identifier, {1, 12, nil}, bar}] = tokenize("~r/foo/ == bar"),
  [{sigil, {1, 1, nil}, 114, [<<"foo">>], "iu", <<"/">>},
   {comp_op, {1, 11, nil}, '=='},
   {identifier, {1, 14, nil}, bar}] = tokenize("~r/foo/iu == bar"),
  [{sigil, {1, 1, nil}, 83, [<<"sigil heredoc\n">>], [], <<"\"\"\"">>}] = tokenize("~S\"\"\"\nsigil heredoc\n\"\"\""),
  [{sigil, {1, 1, nil}, 83, [<<"sigil heredoc\n">>], [], <<"'''">>}] = tokenize("~S'''\nsigil heredoc\n'''").

invalid_sigil_delimiter_test() ->
  {1, 1, "invalid sigil delimiter: ", Message} = tokenize_error("~s\\"),
  true = lists:prefix("\"\\\" (column 3, code point U+005C)", lists:flatten(Message)).
