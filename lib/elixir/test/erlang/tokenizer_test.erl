-module(tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  {ok, _Line, _Column, Result} = elixir_tokenizer:tokenize(String, 1, []),
  Result.

tokenize_error(String) ->
  {error, Error, _, _} = elixir_tokenizer:tokenize(String, 1, []),
  Error.

type_test() ->
  [{number, {1, 1, 2}, 1}, {type_op, {1, 3, 5}, '::'}, {number, {1, 6, 7}, 3}] = tokenize("1 :: 3"),
  [{identifier, {1, 1, 5}, name},
   {'.', {1, 5, 6}},
   {paren_identifier, {1, 6, 8}, '::'},
   {'(', {1, 8, 9}},
   {number, {1, 9, 10}, 3},
   {')', {1, 10, 11}}] = tokenize("name.::(3)").

arithmetic_test() ->
  [{number, {1, 1, 2}, 1}, {dual_op, {1, 3, 4}, '+'}, {number, {1, 5, 6}, 2}, {dual_op, {1, 7, 8}, '+'}, {number, {1, 9, 10}, 3}] = tokenize("1 + 2 + 3").

op_kw_test() ->
  [{atom, {1, 1, 5}, foo}, {dual_op, {1, 5, 6}, '+'}, {atom, {1, 6, 10}, bar}] = tokenize(":foo+:bar").

scientific_test() ->
  [{number, {1, 1, 7}, 0.1}] = tokenize("1.0e-1"),
  [{number, {1, 1, 16}, 1.2345678e-7}] = tokenize("1_234.567_8e-10"),
  {1, "invalid float number ", "1.0e309"} = tokenize_error("1.0e309").

hex_bin_octal_test() ->
  [{number, {1, 1, 5}, 255}] = tokenize("0xFF"),
  [{number, {1, 1, 6}, 255}] = tokenize("0xF_F"),
  [{number, {1, 1, 5}, 63}] = tokenize("0o77"),
  [{number, {1, 1, 6}, 63}] = tokenize("0o7_7"),
  [{number, {1, 1, 5}, 3}] = tokenize("0b11"),
  [{number, {1, 1, 6}, 3}] = tokenize("0b1_1").

unquoted_atom_test() ->
  [{atom, {1, 1, 3}, '+'}] = tokenize(":+"),
  [{atom, {1, 1, 3}, '-'}] = tokenize(":-"),
  [{atom, {1, 1, 3}, '*'}] = tokenize(":*"),
  [{atom, {1, 1, 3}, '/'}] = tokenize(":/"),
  [{atom, {1, 1, 3}, '='}] = tokenize(":="),
  [{atom, {1, 1, 4}, '&&'}] = tokenize(":&&").

quoted_atom_test() ->
  [{atom_unsafe, {1, 1, 11}, [<<"foo bar">>]}] = tokenize(":\"foo bar\"").

oversized_atom_test() ->
  OversizedAtom = [$: | string:copies("a", 256)],
  {1, "atom length must be less than system limit", ":"} = tokenize_error(OversizedAtom).

op_atom_test() ->
  [{atom, {1, 1, 6}, f0_1}] = tokenize(":f0_1").

kw_test() ->
  [{kw_identifier, {1, 1, 4}, do}] = tokenize("do: "),
  [{kw_identifier, {1, 1, 4}, a@}] = tokenize("a@: "),
  [{kw_identifier, {1, 1, 4}, 'A@'}] = tokenize("A@: "),
  [{kw_identifier, {1, 1, 5}, a@b}] = tokenize("a@b: "),
  [{kw_identifier, {1, 1, 5}, 'A@!'}] = tokenize("A@!: "),
  [{kw_identifier, {1, 1, 5}, 'a@!'}] = tokenize("a@!: "),
  [{kw_identifier_unsafe, {1, 1, 10}, [<<"foo bar">>]}] = tokenize("\"foo bar\": ").

integer_test() ->
  [{number, {1, 1, 4}, 123}] = tokenize("123"),
  [{number, {1, 1, 4}, 123}, {';', {1, 4, 5}}] = tokenize("123;"),
  [{eol, {1, 1, 2}}, {number, {3, 1, 4}, 123}] = tokenize("\n\n123"),
  [{number, {1, 3, 6}, 123}, {number, {1, 8, 11}, 234}] = tokenize("  123  234  ").

float_test() ->
  [{number, {1, 1, 5}, 12.3}] = tokenize("12.3"),
  [{number, {1, 1, 5}, 12.3}, {';', {1, 5, 6}}] = tokenize("12.3;"),
  [{eol, {1, 1, 2}}, {number, {3, 1, 5}, 12.3}] = tokenize("\n\n12.3"),
  [{number, {1, 3, 7}, 12.3}, {number, {1, 9, 13}, 23.4}] = tokenize("  12.3  23.4  "),
  OversizedFloat = string:copies("9", 310) ++ ".0",
  {1, "invalid float number ", OversizedFloat} = tokenize_error(OversizedFloat).

comments_test() ->
  [{number, {1, 1, 2}, 1}, {eol, {1, 3, 4}}, {number, {2, 1, 2}, 2}] = tokenize("1 # Comment\n2").

identifier_test() ->
  [{identifier, {1, 1, 4}, abc}] = tokenize("abc "),
  [{identifier, {1, 1, 5}, 'abc?'}] = tokenize("abc?"),
  [{identifier, {1, 1, 5}, 'abc!'}] = tokenize("abc!"),
  [{identifier, {1, 1, 5}, 'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier, {1, 1, 4}, 'a0c'}, {'(', {1, 4, 5}}, {')', {1, 5, 6}}] = tokenize("a0c()"),
  [{paren_identifier, {1, 1, 5}, 'a0c!'}, {'(', {1, 5, 6}}, {')', {1, 6, 7}}] = tokenize("a0c!()").

module_macro_test() ->
  [{identifier, {1, 1, 11}, '__MODULE__'}] = tokenize("__MODULE__").

triple_dot_test() ->
  [{identifier, {1, 1, 4}, '...'}] = tokenize("..."),
  [{'.', {1, 1, 2}}, {identifier, {1, 3, 5}, '..'}] = tokenize(". ..").

dot_test() ->
  [{identifier, {1, 1, 4}, foo},
   {'.', {1, 4, 5}},
   {identifier, {1, 5, 8}, bar},
   {'.', {1, 8, 9}},
   {identifier, {1, 9, 12}, baz}] = tokenize("foo.bar.baz").

dot_keyword_test() ->
 [{identifier, {1, 1, 4}, foo},
  {'.', {1, 4, 5}},
  {identifier, {1, 5, 7}, do}] = tokenize("foo.do").

newline_test() ->
  [{identifier, {1, 1, 4}, foo},
   {'.', {2, 1, 2}},
   {identifier, {2, 2, 5}, bar}]  = tokenize("foo\n.bar"),
  [{number, {1, 1, 2}, 1},
   {two_op, {2, 1, 3}, '++'},
   {number, {2, 3, 4}, 2}]  = tokenize("1\n++2").

dot_newline_operator_test() ->
  [{identifier, {1, 1, 4}, foo},
   {'.', {2, 4, 5}},
   {identifier, {2, 1, 2}, '+'},
   {number, {2, 2, 3}, 1}] = tokenize("foo.\n+1"),
  [{identifier, {1, 1, 4}, foo},
   {'.', {2, 4, 5}},
   {identifier, {2, 1, 2}, '+'},
   {number, {2, 2, 3}, 1}] = tokenize("foo.#bar\n+1").

aliases_test() ->
  [{'aliases', {1, 1, 4}, ['Foo']}] = tokenize("Foo"),
  [{'aliases', {1, 1, 4}, ['Foo']},
   {'.', {1, 4, 5}},
   {'aliases', {1, 5, 8}, ['Bar']},
   {'.', {1, 8, 9}},
   {'aliases', {1, 9, 12}, ['Baz']}] = tokenize("Foo.Bar.Baz").

string_test() ->
  [{bin_string, {1, 1, 6}, [<<"foo">>]}] = tokenize("\"foo\""),
  [{bin_string, {1, 1, 6}, [<<"f\"">>]}] = tokenize("\"f\\\"\""),
  [{list_string, {1, 1, 6}, [<<"foo">>]}] = tokenize("'foo'").

empty_string_test() ->
  [{bin_string, {1, 1, 3}, [<<>>]}] = tokenize("\"\""),
  [{list_string, {1, 1, 3}, [<<>>]}] = tokenize("''").

addadd_test() ->
  [{identifier, {1, 1, 2}, x}, {two_op, {1, 3, 5}, '++'}, {identifier, {1, 6, 7}, y}] = tokenize("x ++ y").

space_test() ->
  [{op_identifier, {1, 1, 4}, foo}, {dual_op, {1, 5, 6}, '-'}, {number, {1, 6, 7}, 2}] = tokenize("foo -2"),
  [{op_identifier, {1, 1, 4}, foo}, {dual_op, {1, 6, 7}, '-'}, {number, {1, 7, 8}, 2}] = tokenize("foo  -2").

chars_test() ->
  [{char, {1, 1, 3}, 97}] = tokenize("?a"),
  [{char, {1, 1, 3}, 99}] = tokenize("?c"),
  [{char, {1, 1, 4}, 0}]  = tokenize("?\\0"),
  [{char, {1, 1, 4}, 7}]  = tokenize("?\\a"),
  [{char, {1, 1, 4}, 10}] = tokenize("?\\n"),
  [{char, {1, 1, 4}, 92}] = tokenize("?\\\\").

interpolation_test() ->
  [{bin_string, {1, 1, 9}, [<<"f">>,
    {{1, 3, 8}, [{identifier, {1, 5, 7}, oo}]}]},
   {two_op, {1, 10, 12}, '<>'}, {bin_string, {1, 13, 15},
    [<<>>]}] = tokenize("\"f#{oo}\" <> \"\"").

capture_test() ->
  [{capture_op, {1, 1, 2}, '&'},
   {identifier, {1, 2, 4}, '||'},
   {mult_op,    {1, 4, 5}, '/'},
   {number,     {1, 5, 6}, 2}] = tokenize("&||/2"),
  [{capture_op, {1, 1, 2}, '&'},
   {identifier, {1, 2, 4}, 'or'},
   {mult_op,    {1, 4, 5}, '/'},
   {number,     {1, 5, 6}, 2}] = tokenize("&or/2"),
  [{capture_op,{1,1,2},'&'},
   {unary_op,{1,2,5},'not'},
   {number,{1,6,7},1},
   {',',{1,7,8}},
   {number,{1,9,10},2}] = tokenize("&not 1, 2").

vc_merge_conflict_test() ->
  {1, "found an unexpected version control marker, please resolve the conflicts: ", "<<<<<<< HEAD"} =
    tokenize_error("<<<<<<< HEAD\n[1, 2, 3]").

invalid_sigil_delimiter_test() ->
  {1, "invalid sigil delimiter: ", Message} = tokenize_error("~s\\"),
  true = lists:prefix("\"\\\" (column 3, codepoint U+005C)", lists:flatten(Message)).
