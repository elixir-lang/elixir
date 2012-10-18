-module(tokenizer_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  { ok, Result } = elixir_tokenizer:tokenize(#elixir_tokenizer_context{ string = String, line = 1, file = <<"nofile">> }),
  Result.

colon_colon_test() ->
  [{number,1,1},{'::',1},{number,1,3}] = tokenize("1 :: 3"),
  [{identifier,1,foo},
   {'.',1},
   {paren_identifier,1,'::'},
   {'(',1},
   {number,1,3},
   {')',1}] = tokenize("foo.::(3)").

arithmetic_test() ->
  [{number,1,1},{'+',1},{number,1,2},{'+',1},{number,1,3}] = tokenize("1 + 2 + 3").

scientific_test() ->
  [{number, 1, 0.1}] = tokenize("1.0e-1").

hex_bin_octal_test() ->
  [{number,1,255}] = tokenize("0xFF"),
  [{number,1,255}] = tokenize("0Xff"),
  [{number,1,63}] = tokenize("0o77"),
  [{number,1,63}] = tokenize("0O77"),
  [{number,1,3}] = tokenize("0b11"),
  [{number,1,3}] = tokenize("0B11").

unquoted_atom_test() ->
  [{atom, 1, ['+']}] = tokenize(":+"),
  [{atom, 1, ['-']}] = tokenize(":-"),
  [{atom, 1, ['*']}] = tokenize(":*"),
  [{atom, 1, ['/']}] = tokenize(":/"),
  [{atom, 1, ['=']}] = tokenize(":="),
  [{atom, 1, ['&&']}] = tokenize(":&&").

op_atom_test() ->
  [{atom,1,[f0_1]}] = tokenize(":f0_1").

kw_test() ->
  [{kw_identifier,1,do}] = tokenize("do: ").

integer_test() ->
  [{number, 1, 123}] = tokenize("123"),
  [{number, 1, 123},{eol, 1, $;}] = tokenize("123;"),
  [{eol, 1, $\n}, {number, 3, 123}] = tokenize("\n\n123"),
  [{number, 1, 123}, {number, 1, 234}] = tokenize("  123  234  ").

float_test() ->
  [{number, 1, 12.3}] = tokenize("12.3"),
  [{number, 1, 12.3},{eol, 1, $;}] = tokenize("12.3;"),
  [{eol, 1, $\n}, {number, 3, 12.3}] = tokenize("\n\n12.3"),
  [{number, 1, 12.3}, {number, 1, 23.4}] = tokenize("  12.3  23.4  ").

comments_test() ->
  [{number, 1, 1},{eol, 1, $\n},{number,2,2}] = tokenize("1 # Comment\n2").

identifier_test() ->
  [{identifier,1,abc}] = tokenize("abc "),
  [{punctuated_identifier,1,'abc?'}] = tokenize("abc?"),
  [{punctuated_identifier,1,'abc!'}] = tokenize("abc!"),
  [{punctuated_identifier,1,'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier,1,'a0c'},{'(',1},{')',1}] = tokenize("a0c()"),
  [{paren_identifier,1,'a0c!'},{'(',1},{')',1}] = tokenize("a0c!()").

module_macro_test() ->
    [{identifier,1,'__MODULE__'}] = tokenize("__MODULE__").

file_macro_test() ->
    [{identifier,1,'__FILE__'}] = tokenize("__FILE__").

dot_test() ->
  [{identifier,1,foo},
   {'.',1},
   {identifier,1,bar},
   {'.',1},
   {identifier,1,baz}] = tokenize("foo.bar.baz").

newline_test() ->
 [{identifier,1,foo},
  {'.',2},
  {identifier,2,bar}]  = tokenize("foo\n.bar"),
  [{number,1,1},
   {'++',2},
   {number,2,2}]  = tokenize("1\n++2").

aliases_test() ->
  [{'__aliases__',1,['Foo']}] = tokenize("Foo"),
  [{'__aliases__',1,['Foo']},
   {'.',1},
   {'__aliases__',1,['Bar']},
   {'.',1},
   {'__aliases__',1,['Baz']}] = tokenize("Foo.Bar.Baz").

string_test() ->
  [{bin_string,1,[<<"foo">>]}] = tokenize("\"foo\""),
  [{list_string,1,[<<"foo">>]}] = tokenize("'foo'").

empty_string_test() ->
  [{bin_string,1,[<<>>]}] = tokenize("\"\""),
  [{list_string,1,[<<>>]}] = tokenize("''").

default_test() ->
  [{identifier,1,x},{'//',1},{number,1,1}] = tokenize("x // 1").

addadd_test() ->
  [{identifier,1,x},{'++',1},{identifier,1,y}] = tokenize("x ++ y").

chars_test() ->
  [{number,1,97}] = tokenize("?a"),
  [{number,1,99}] = tokenize("?c"),
  [{number,1,10}] = tokenize("?\\n"),
  [{number,1,92}] = tokenize("?\\\\").
