-module(tokenizer_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  { ok, _Line, Result } = elixir_tokenizer:tokenize(String, 1, []),
  Result.

tokenize_error(String) ->
  { error, Error, _, _ } = elixir_tokenizer:tokenize(String, 1, []),
  Error.

type_test() ->
  [{number,1,1},{type_op,1,'::'},{number,1,3}] = tokenize("1 :: 3"),
  [{identifier,1,foo},
   {'.',1},
   {paren_identifier,1,'::'},
   {'(',1},
   {number,1,3},
   {')',1}] = tokenize("foo.::(3)").

arithmetic_test() ->
  [{number,1,1},{dual_op,1,'+'},{number,1,2},{dual_op,1,'+'},{number,1,3}] = tokenize("1 + 2 + 3").

op_kw_test() ->
  [{atom,1,foo},{dual_op,1,'+'},{atom,1,bar}] = tokenize(":foo+:bar").

scientific_test() ->
  [{number, 1, 0.1}] = tokenize("1.0e-1").

hex_bin_octal_test() ->
  [{number,1,255}] = tokenize("0xFF"),
  [{number,1,255}] = tokenize("0Xff"),
  [{number,1,63}] = tokenize("077"),
  [{number,1,63}] = tokenize("077"),
  [{number,1,3}] = tokenize("0b11"),
  [{number,1,3}] = tokenize("0B11").

unquoted_atom_test() ->
  [{atom, 1, '+'}] = tokenize(":+"),
  [{atom, 1, '-'}] = tokenize(":-"),
  [{atom, 1, '*'}] = tokenize(":*"),
  [{atom, 1, '/'}] = tokenize(":/"),
  [{atom, 1, '='}] = tokenize(":="),
  [{atom, 1, '&&'}] = tokenize(":&&").

quoted_atom_test() ->
  [{atom_string, 1, false, [<<"foo bar">>]}] = tokenize(":\"foo bar\"").

oversized_atom_test() ->
  OversizedAtom = [$:|string:copies("a", 256)],
  { 1, "atom length must be less than system limit", ":" } = tokenize_error(OversizedAtom).

op_atom_test() ->
  [{atom,1,f0_1}] = tokenize(":f0_1").

kw_test() ->
  [{kw_identifier, 1, do}] = tokenize("do: "),
  [{kw_identifier_string, 1, false, [<<"foo bar">>]}] = tokenize("\"foo bar\": ").

integer_test() ->
  [{number, 1, 123}] = tokenize("123"),
  [{number, 1, 123},{eol, 1, ';'}] = tokenize("123;"),
  [{eol, 1, newline}, {number, 3, 123}] = tokenize("\n\n123"),
  [{number, 1, 123}, {number, 1, 234}] = tokenize("  123  234  ").

float_test() ->
  [{number, 1, 12.3}] = tokenize("12.3"),
  [{number, 1, 12.3},{eol, 1, ';'}] = tokenize("12.3;"),
  [{eol, 1, newline}, {number, 3, 12.3}] = tokenize("\n\n12.3"),
  [{number, 1, 12.3}, {number, 1, 23.4}] = tokenize("  12.3  23.4  ").

comments_test() ->
  [{number, 1, 1},{eol, 1, newline},{number,2,2}] = tokenize("1 # Comment\n2").

identifier_test() ->
  [{identifier,1,abc}] = tokenize("abc "),
  [{identifier,1,'abc?'}] = tokenize("abc?"),
  [{identifier,1,'abc!'}] = tokenize("abc!"),
  [{identifier,1,'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier,1,'a0c'},{'(',1},{')',1}] = tokenize("a0c()"),
  [{paren_identifier,1,'a0c!'},{'(',1},{')',1}] = tokenize("a0c!()").

module_macro_test() ->
  [{identifier,1,'__MODULE__'}] = tokenize("__MODULE__").

triple_dot_test() ->
  [{identifier,1,'...'}] = tokenize("..."),
  [{'.',1},{identifier,1,'..'}] = tokenize(". ..").

dot_test() ->
  [{identifier,1,foo},
   {'.',1},
   {identifier,1,bar},
   {'.',1},
   {identifier,1,baz}] = tokenize("foo.bar.baz").

dot_keyword_test() ->
 [{identifier,1,foo},
  {'.',1},
  {identifier,1,do}] = tokenize("foo.do").

newline_test() ->
 [{identifier,1,foo},
  {'.',2},
  {identifier,2,bar}]  = tokenize("foo\n.bar"),
  [{number,1,1},
   {two_op,2,'++'},
   {number,2,2}]  = tokenize("1\n++2").

aliases_test() ->
  [{'aliases',1,['Foo']}] = tokenize("Foo"),
  [{'aliases',1,['Foo']},
   {'.',1},
   {'aliases',1,['Bar']},
   {'.',1},
   {'aliases',1,['Baz']}] = tokenize("Foo.Bar.Baz").

string_test() ->
  [{bin_string,1,[<<"foo">>]}] = tokenize("\"foo\""),
  [{list_string,1,[<<"foo">>]}] = tokenize("'foo'").

empty_string_test() ->
  [{bin_string,1,[<<>>]}] = tokenize("\"\""),
  [{list_string,1,[<<>>]}] = tokenize("''").

addadd_test() ->
  [{identifier,1,x},{two_op,1,'++'},{identifier,1,y}] = tokenize("x ++ y").

chars_test() ->
  [{number,1,97}]      = tokenize("?a"),
  [{number,1,99}]      = tokenize("?c"),
  [{number,1,7}]       = tokenize("?\\a"),
  [{number,1,10}]      = tokenize("?\\n"),
  [{number,1,92}]      = tokenize("?\\\\"),
  [{number,1,10}]      = tokenize("?\\xa"),
  [{number,1,26}]      = tokenize("?\\X1a"),
  [{number,1,6}]       = tokenize("?\\6"),
  [{number,1,49}]      = tokenize("?\\61"),
  [{number,1,255}]     = tokenize("?\\377"),
  [{number,1,10}]      = tokenize("?\\x{a}"),
  [{number,1,171}]     = tokenize("?\\x{ab}"),
  [{number,1,2748}]    = tokenize("?\\x{abc}"),
  [{number,1,43981}]   = tokenize("?\\x{abcd}"),
  [{number,1,703710}]  = tokenize("?\\x{abcde}"),
  [{number,1,1092557}] = tokenize("?\\x{10abcd}").
