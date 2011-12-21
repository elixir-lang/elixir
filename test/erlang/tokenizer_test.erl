-module(tokenizer_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  { ok, Result } = elixir_tokenizer:tokenize(String, 1),
  Result.

arithmetic_test() ->
  [{number,1,1},{'+',1},{number,1,2},{'+',1},{number,1,3}] = tokenize("1 + 2 + 3").

op_call_test() ->
  [{call_op,1,'+'},{'(',1},{number,1,1},{',',1},{number,1,2},{')',1}] = tokenize("+(1, 2)").

unquoted_atom_test() ->
  [{atom, 1, ['+']}] = tokenize(":+"),
  [{atom, 1, ['-']}] = tokenize(":-"),
  [{atom, 1, ['*']}] = tokenize(":*"),
  [{atom, 1, ['/']}] = tokenize(":/"),
  [{atom, 1, ['=']}] = tokenize(":="),
  [{atom, 1, ['&&']}] = tokenize(":&&").

op_atom_test() ->
  [{atom,1,[f0_1]}] = tokenize(":f0_1").

kv_test() ->
  [{kv_identifier,1,do}] = tokenize("do:"),
  [{kv_identifier,1,'+'}] = tokenize("+:"),
  [{kv_identifier,1,'||'}] = tokenize("||:"),
  [{identifier,1,def},{kv_identifier,1,'+'}] = tokenize("def +:").

integer_test() ->
  [{number, 1, 123}] = tokenize("123"),
  [{number, 1, 123},{eol, 1}] = tokenize("123;"),
  [{eol, 1}, {number, 3, 123}] = tokenize("\n\n123"),
  [{number, 1, 123}, {number, 1, 234}] = tokenize("  123  234  ").

float_test() ->
  [{number, 1, 12.3}] = tokenize("12.3"),
  [{number, 1, 12.3},{eol, 1}] = tokenize("12.3;"),
  [{eol, 1}, {number, 3, 12.3}] = tokenize("\n\n12.3"),
  [{number, 1, 12.3}, {number, 1, 23.4}] = tokenize("  12.3  23.4  ").

comments_test() ->
    [{number, 1, 1},{eol, 1},{number,2,2}] = tokenize("1 # Comment\n2"),
    [{number, 1, 12.3},{eol, 1}] = tokenize("12.3;"),
    [{eol, 1}, {number, 3, 12.3}] = tokenize("\n\n12.3"),
    [{number, 1, 12.3}, {number, 1, 23.4}] = tokenize("  12.3  23.4  ").

identifier_test() ->
  [{identifier,1,abc}] = tokenize("abc "),
  [{punctuated_identifier,1,'abc?'}] = tokenize("abc?"),
  [{punctuated_identifier,1,'abc!'}] = tokenize("abc!"),
  [{punctuated_identifier,1,'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier,1,'a0c'},{'(',1},{')',1}] = tokenize("a0c()"),
  [{paren_identifier,1,'a0c!'},{'(',1},{')',1}] = tokenize("a0c!()").

dot_test() ->
  [{identifier,1,foo},
   {'.',1},
   {identifier,1,bar},
   {'.',1},
   {identifier,1,baz}] = tokenize("foo.bar.baz").

ref_test() ->
  [{ref,1,Foo}] = tokenize("Foo"),
  [{ref,1,Foo},
   {'::',1},
   {ref,1,Bar},
   {'::',1},
   {ref,1,Baz}] = tokenize("Foo::Bar::Baz").

string_test() ->
  [{string,1,["foo"]}] = tokenize("\"foo\"").