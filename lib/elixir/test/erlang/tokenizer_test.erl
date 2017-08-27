-module(tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  tokenize(String, []).

tokenize(String, Opts) ->
  {ok, _Line, _Column, Result} = elixir_tokenizer:tokenize(String, 1, Opts),
  Result.

tokenize_error(String) ->
  {error, Error, _, _} = elixir_tokenizer:tokenize(String, 1, []),
  Error.

type_test() ->
  [{int, {1, {1, 2}, 1}, "1"},
   {type_op, {1, {3, 5}, nil}, '::'},
   {int, {1, {6, 7}, 3}, "3"}] = tokenize("1 :: 3"),
  [{identifier, {1, {1, 5}, nil}, name},
   {'.', {1, {5, 6}, nil}},
   {paren_identifier, {1, {6, 8}, nil}, '::'},
   {'(', {1, {8, 9}, nil}},
   {int, {1, {9, 10}, 3}, "3"},
   {')', {1, {10, 11}, nil}}] = tokenize("name.::(3)").

arithmetic_test() ->
  [{int, {1, {1, 2}, 1}, "1"},
   {dual_op, {1, {3, 4}, nil}, '+'},
   {int, {1, {5, 6}, 2}, "2"},
   {dual_op, {1, {7, 8}, nil}, '+'},
   {int, {1, {9, 10}, 3}, "3"}] = tokenize("1 + 2 + 3").

op_kw_test() ->
  [{atom, {1, {1, 5}, nil}, foo},
   {dual_op, {1, {5, 6}, nil}, '+'},
   {atom, {1, {6, 10}, nil}, bar}] = tokenize(":foo+:bar").

scientific_test() ->
  [{float, {1, {1, 7}, 0.1}, "1.0e-1"}] = tokenize("1.0e-1"),
  [{float, {1, {1, 7}, 0.1}, "1.0E-1"}] = tokenize("1.0E-1"),
  [{float, {1, {1, 16}, 1.2345678e-7}, "1234.5678e-10"}] = tokenize("1_234.567_8e-10"),
  {1, "invalid float number ", "1.0e309"} = tokenize_error("1.0e309").

hex_bin_octal_test() ->
  [{int, {1, {1, 5}, 255}, "0xFF"}] = tokenize("0xFF"),
  [{int, {1, {1, 6}, 255}, "0xFF"}] = tokenize("0xF_F"),
  [{int, {1, {1, 5}, 63}, "0o77"}] = tokenize("0o77"),
  [{int, {1, {1, 6}, 63}, "0o77"}] = tokenize("0o7_7"),
  [{int, {1, {1, 5}, 3}, "0b11"}] = tokenize("0b11"),
  [{int, {1, {1, 6}, 3}, "0b11"}] = tokenize("0b1_1").

unquoted_atom_test() ->
  [{atom, {1, {1, 3}, nil}, '+'}] = tokenize(":+"),
  [{atom, {1, {1, 3}, nil}, '-'}] = tokenize(":-"),
  [{atom, {1, {1, 3}, nil}, '*'}] = tokenize(":*"),
  [{atom, {1, {1, 3}, nil}, '/'}] = tokenize(":/"),
  [{atom, {1, {1, 3}, nil}, '='}] = tokenize(":="),
  [{atom, {1, {1, 4}, nil}, '&&'}] = tokenize(":&&").

quoted_atom_test() ->
  [{atom_unsafe, {1, {1, 11}, nil}, [<<"foo bar">>]}] = tokenize(":\"foo bar\"").

oversized_atom_test() ->
  OversizedAtom = [$: | string:copies("a", 256)],
  {1, "atom length must be less than system limit", ":"} = tokenize_error(OversizedAtom).

op_atom_test() ->
  [{atom, {1, {1, 6}, nil}, f0_1}] = tokenize(":f0_1").

kw_test() ->
  [{kw_identifier, {1, {1, 4}, nil}, do}] = tokenize("do: "),
  [{kw_identifier, {1, {1, 4}, nil}, a@}] = tokenize("a@: "),
  [{kw_identifier, {1, {1, 4}, nil}, 'A@'}] = tokenize("A@: "),
  [{kw_identifier, {1, {1, 5}, nil}, a@b}] = tokenize("a@b: "),
  [{kw_identifier, {1, {1, 5}, nil}, 'A@!'}] = tokenize("A@!: "),
  [{kw_identifier, {1, {1, 5}, nil}, 'a@!'}] = tokenize("a@!: "),
  [{kw_identifier_unsafe, {1, {1, 10}, nil}, [<<"foo bar">>]}] = tokenize("\"foo bar\": ").

int_test() ->
  [{int, {1, {1, 4}, 123}, "123"}] = tokenize("123"),
  [{int, {1, {1, 4}, 123}, "123"}, {';', {1, {4, 5}, nil}}] = tokenize("123;"),
  [{eol, {1, {1, 2}, nil}}, {int, {3, {1, 4}, 123}, "123"}] = tokenize("\n\n123"),
  [{int, {1, {3, 6}, 123}, "123"}, {int, {1, {8, 11}, 234}, "234"}] = tokenize("  123  234  "),
  [{int, {1, {1, 4}, 7}, "007"}] = tokenize("007"),
  [{int, {1, {1, 8}, 100000}, "0100000"}] = tokenize("0100000").

float_test() ->
  [{float, {1, {1, 5}, 12.3}, "12.3"}] = tokenize("12.3"),
  [{float, {1, {1, 5}, 12.3}, "12.3"}, {';', {1, {5, 6}, nil}}] = tokenize("12.3;"),
  [{eol, {1, {1, 2}, nil}}, {float, {3, {1, 5}, 12.3}, "12.3"}] = tokenize("\n\n12.3"),
  [{float, {1, {3, 7}, 12.3}, "12.3"}, {float, {1, {9, 13}, 23.4}, "23.4"}] = tokenize("  12.3  23.4  "),
  [{float, {1, {1, 11}, 12.3}, "0012.300"}] = tokenize("00_12.3_00"),
  OversizedFloat = string:copies("9", 310) ++ ".0",
  {1, "invalid float number ", OversizedFloat} = tokenize_error(OversizedFloat).

comments_test() ->
  [{int, {1, {1, 2}, 1}, "1"},
   {eol, {1, {3, 4}, nil}},
   {int, {2, {1, 2}, 2}, "2"}] = tokenize("1 # Comment\n2"),
  [{int, {1, {1, 2}, 1}, "1"},
   {comment, {1, {3, 12}, nil}, "# Comment"},
   {eol, {1, {12, 13}, nil}},
   {int, {2, {1, 2}, 2}, "2"}] = tokenize("1 # Comment\n2", [{preserve_comments, true}]),
  [{comment, {1, {1, 10}, nil}, "# Comment"}] = tokenize("# Comment", [{preserve_comments, true}]).

identifier_test() ->
  [{identifier, {1, {1, 4}, nil}, abc}] = tokenize("abc "),
  [{identifier, {1, {1, 5}, nil}, 'abc?'}] = tokenize("abc?"),
  [{identifier, {1, {1, 5}, nil}, 'abc!'}] = tokenize("abc!"),
  [{identifier, {1, {1, 5}, nil}, 'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier, {1, {1, 4}, nil}, 'a0c'}, {'(', {1, {4, 5}, nil}}, {')', {1, {5, 6}, nil}}] = tokenize("a0c()"),
  [{paren_identifier, {1, {1, 5}, nil}, 'a0c!'}, {'(', {1, {5, 6}, nil}}, {')', {1, {6, 7}, nil}}] = tokenize("a0c!()").

module_macro_test() ->
  [{identifier, {1, {1, 11}, nil}, '__MODULE__'}] = tokenize("__MODULE__").

triple_dot_test() ->
  [{identifier, {1, {1, 4}, nil}, '...'}] = tokenize("..."),
  [{'.', {1, {1, 2}, nil}}, {identifier, {1, {3, 5}, nil}, '..'}] = tokenize(". ..").

dot_test() ->
  [{identifier, {1, {1, 4}, nil}, foo},
   {'.', {1, {4, 5}, nil}},
   {identifier, {1, {5, 8}, nil}, bar},
   {'.', {1, {8, 9}, nil}},
   {identifier, {1, {9, 12}, nil}, baz}] = tokenize("foo.bar.baz").

dot_keyword_test() ->
 [{identifier, {1, {1, 4}, nil}, foo},
  {'.', {1, {4, 5}, nil}},
  {identifier, {1, {5, 7}, nil}, do}] = tokenize("foo.do").

newline_test() ->
  [{identifier, {1, {1, 4}, nil}, foo},
   {'.', {2, {1, 2}, nil}},
   {identifier, {2, {2, 5}, nil}, bar}]  = tokenize("foo\n.bar"),
  [{int, {1, {1, 2}, 1}, "1"},
   {two_op, {2, {1, 3}, nil}, '++'},
   {int, {2, {3, 4}, 2}, "2"}]  = tokenize("1\n++2").

dot_newline_operator_test() ->
  [{identifier, {1, {1, 4}, nil}, foo},
   {'.', {1, {4, 5}, nil}},
   {identifier, {2, {1, 2}, nil}, '+'},
   {int, {2, {2, 3}, 1}, "1"}] = tokenize("foo.\n+1"),
  [{identifier, {1, {1, 4}, nil}, foo},
   {'.', {1, {4, 5}, nil}},
   {identifier, {2, {1, 2}, nil}, '+'},
   {int, {2, {2, 3}, 1}, "1"}] = tokenize("foo.#bar\n+1"),
  [{identifier, {1, {1, 4}, nil}, foo},
   {'.', {1, {4, 5}, nil}},
   {comment, {1, {5, 9}, nil}, "#bar"},
   {identifier, {2, {1, 2}, nil}, '+'},
   {int, {2, {2, 3}, 1}, "1"}] = tokenize("foo.#bar\n+1", [{preserve_comments, true}]).

aliases_test() ->
  [{'aliases', {1, {1, 4}, nil}, ['Foo']}] = tokenize("Foo"),
  [{'aliases', {1, {1, 4}, nil}, ['Foo']},
   {'.', {1, {4, 5}, nil}},
   {'aliases', {1, {5, 8}, nil}, ['Bar']},
   {'.', {1, {8, 9}, nil}},
   {'aliases', {1, {9, 12}, nil}, ['Baz']}] = tokenize("Foo.Bar.Baz").

string_test() ->
  [{bin_string, {1, {1, 6}, nil}, [<<"foo">>]}] = tokenize("\"foo\""),
  [{bin_string, {1, {1, 6}, nil}, [<<"f\"">>]}] = tokenize("\"f\\\"\""),
  [{list_string, {1, {1, 6}, nil}, [<<"foo">>]}] = tokenize("'foo'").

empty_string_test() ->
  [{bin_string, {1, {1, 3}, nil}, [<<>>]}] = tokenize("\"\""),
  [{list_string, {1, {1, 3}, nil}, [<<>>]}] = tokenize("''").

addadd_test() ->
  [{identifier, {1, {1, 2}, nil}, x},
   {two_op, {1, {3, 5}, nil}, '++'},
   {identifier, {1, {6, 7}, nil}, y}] = tokenize("x ++ y").

space_test() ->
  [{op_identifier, {1, {1, 4}, nil}, foo},
   {dual_op, {1, {5, 6}, nil}, '-'},
   {int, {1, {6, 7}, 2}, "2"}] = tokenize("foo -2"),
  [{op_identifier, {1, {1, 4}, nil}, foo},
   {dual_op, {1, {6, 7}, nil}, '-'},
   {int, {1, {7, 8}, 2}, "2"}] = tokenize("foo  -2").

chars_test() ->
  [{int, {1, {1, 3}, 97}, "?a"}] = tokenize("?a"),
  [{int, {1, {1, 3}, 99}, "?c"}] = tokenize("?c"),
  [{int, {1, {1, 4}, 0}, "?\\0"}]  = tokenize("?\\0"),
  [{int, {1, {1, 4}, 7}, "?\\a"}]  = tokenize("?\\a"),
  [{int, {1, {1, 4}, 10}, "?\\n"}] = tokenize("?\\n"),
  [{int, {1, {1, 4}, 92}, "?\\\\"}] = tokenize("?\\\\").

interpolation_test() ->
  [{bin_string, {1, {1, 9}, nil}, [<<"f">>, {{1, {3, 8}, nil}, [{identifier, {1, {5, 7}, nil}, oo}]}]},
   {two_op, {1, {10, 12}, nil}, '<>'},
   {bin_string, {1, {13, 15}, nil}, [<<>>]}] = tokenize("\"f#{oo}\" <> \"\"").

capture_test() ->
  [{capture_op, {1, {1, 2}, nil}, '&'},
   {identifier, {1, {2, 4}, nil}, '||'},
   {mult_op, {1, {4, 5}, nil}, '/'},
   {int, {1, {5, 6}, 2}, "2"}] = tokenize("&||/2"),
  [{capture_op, {1, {1, 2}, nil}, '&'},
   {identifier, {1, {2, 4}, nil}, 'or'},
   {mult_op, {1, {4, 5}, nil}, '/'},
   {int, {1, {5, 6}, 2}, "2"}] = tokenize("&or/2"),
  [{capture_op, {1, {1, 2}, nil}, '&'},
   {unary_op, {1, {2, 5}, nil}, 'not'},
   {int, {1, {6, 7}, 1}, "1"},
   {',', {1, {7, 8}, nil}},
   {int, {1, {9, 10}, 2}, "2"}] = tokenize("&not 1, 2").

vc_merge_conflict_test() ->
  {1, "found an unexpected version control marker, please resolve the conflicts: ", "<<<<<<< HEAD"} =
    tokenize_error("<<<<<<< HEAD\n[1, 2, 3]").

sigil_terminator_test() ->
  [{sigil, {1, {1, 8}, nil}, 114, [<<"foo">>], [], <<"/">>}] = tokenize("~r/foo/"),
  [{sigil, {1, {1, 8}, nil}, 114, [<<"foo">>], [], <<"[">>}] = tokenize("~r[foo]"),
  [{sigil, {1, {1, 8}, nil}, 114, [<<"foo">>], [], <<"\"">>}] = tokenize("~r\"foo\""),
  [{sigil, {1, {1, 1}, nil}, 83, [<<"sigil heredoc\n">>], [], <<"\"\"\"">>}] = tokenize("~S\"\"\"\nsigil heredoc\n\"\"\""),
  [{sigil, {1, {1, 1}, nil}, 83, [<<"sigil heredoc\n">>], [], <<"'''">>}] = tokenize("~S'''\nsigil heredoc\n'''").

invalid_sigil_delimiter_test() ->
  {1, "invalid sigil delimiter: ", Message} = tokenize_error("~s\\"),
  true = lists:prefix("\"\\\" (column 3, codepoint U+005C)", lists:flatten(Message)).
