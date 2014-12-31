-module(tokenizer_test).
-include_lib("eunit/include/eunit.hrl").
% -import(elixir_tokenizer, [tokenize/3]).

% main(_) ->
%   arithmetic_test(),
%   chars_test(),
%   invalid_space_test(),
%   space_test(),
%   addadd_test(),
%   empty_string_test(),
%   string_test(),
%   aliases_test(),
%   newline_test(),
%   dot_test(), dot_keyword_test(),
%   module_macro_test(), triple_dot_test(),
%   comments_test(), identifier_test(),
%   integer_test(), float_test(),
%   op_atom_test(), kw_test(),
%   oversized_atom_test(),
%   quoted_atom_test(),
%   hex_bin_octal_test(),
%   unquoted_atom_test(),
%   op_kw_test(), scientific_test(),
%   type_test().



tokenize(String) ->
  {ok, _Line, _Column, Result} = elixir_tokenizer:tokenize(String, 1, []),
  Result.

tokenize_error(String) ->
  {error, Error, _, _} = elixir_tokenizer:tokenize(String, 1, []),
  Error.

type_test() ->
  [{number,[1,0,1],1},{type_op,[1,2,4],'::'},{number,[1,5,6],3}] = tokenize("1 :: 3"),
  [{identifier,[1,0,4],name},
   {'.',[1,4,5]},
   {paren_identifier,[1,5,7],'::'},
   {'(',[1,7,8]},
   {number,[1,8,9],3},
   {')',[1,9,10]}] = tokenize("name.::(3)").

arithmetic_test() ->
  [{number,[1,0,1],1},{dual_op,[1,2,3],'+'},{number,[1,4,5],2},{dual_op,[1,6,7],'+'},{number,[1,8,9],3}] = tokenize("1 + 2 + 3").

op_kw_test() ->
  [{atom,[1,0,4],foo},{dual_op,[1,4,5],'+'},{atom,[1,5,9],bar}] = tokenize(":foo+:bar").

scientific_test() ->
  [{number, [1,0,6], 0.1}] = tokenize("1.0e-1").

hex_bin_octal_test() ->
  [{number,[1,0,4],255}] = tokenize("0xFF"),
  [{number,[1,0,4],63}] = tokenize("0o77"),
  [{number,[1,0,4],3}] = tokenize("0b11").

unquoted_atom_test() ->
  [{atom, [1,0,2], '+'}] = tokenize(":+"),
  [{atom, [1,0,2], '-'}] = tokenize(":-"),
  [{atom, [1,0,2], '*'}] = tokenize(":*"),
  [{atom, [1,0,2], '/'}] = tokenize(":/"),
  [{atom, [1,0,2], '='}] = tokenize(":="),
  [{atom, [1,0,3], '&&'}] = tokenize(":&&").

quoted_atom_test() ->
  [{atom_unsafe, [1,0,10], [<<"foo bar">>]}] = tokenize(":\"foo bar\"").

oversized_atom_test() ->
  OversizedAtom = [$:|string:copies("a", 256)],
  {1, "atom length must be less than system limit", ":"} = tokenize_error(OversizedAtom).

op_atom_test() ->
  [{atom,[1,0,5],f0_1}] = tokenize(":f0_1").

kw_test() ->
  [{kw_identifier, [1,0,2], do}] = tokenize("do: "),
  [{kw_identifier_unsafe, [1, 0, 9], [<<"foo bar">>]}] = tokenize("\"foo bar\": ").

integer_test() ->
  [{number, [1,0,3], 123}] = tokenize("123"),
  [{number, [1,0,3], 123},{';', [1,3,4]}] = tokenize("123;"),
  [{eol, [1,0,1]}, {number, [3,0,3], 123}] = tokenize("\n\n123"),
  [{number, [1,2,5], 123}, {number, [1,7,10], 234}] = tokenize("  123  234  ").

float_test() ->
  [{number, [1,0,4], 12.3}] = tokenize("12.3"),
  [{number, [1,0,4], 12.3},{';', [1,4,5]}] = tokenize("12.3;"),
  [{eol, [1,0,1]}, {number, [3,0,4], 12.3}] = tokenize("\n\n12.3"),
  [{number, [1,2,6], 12.3}, {number, [1,8,12], 23.4}] = tokenize("  12.3  23.4  ").

comments_test() ->
  [{number, [1,0,1], 1},{eol, [1,2,3]},{number,[2,0,1],2}] = tokenize("1 # Comment\n2").

identifier_test() ->
  [{identifier,[1,0,3],abc}] = tokenize("abc "),
  [{identifier,[1,0,4],'abc?'}] = tokenize("abc?"),
  [{identifier,[1,0,4],'abc!'}] = tokenize("abc!"),
  [{identifier,[1,0,4],'a0c!'}] = tokenize("a0c!"),
  [{paren_identifier,[1,0,3],'a0c'},{'(',[1,3,4]},{')',[1,4,5]}] = tokenize("a0c()"),
  [{paren_identifier,[1,0,4],'a0c!'},{'(',[1,4,5]},{')',[1,5,6]}] = tokenize("a0c!()").

module_macro_test() ->
  [{identifier,[1,0,10],'__MODULE__'}] = tokenize("__MODULE__").

triple_dot_test() ->
  [{identifier,[1,0,3],'...'}] = tokenize("..."),
  [{'.',[1,0,1]},{identifier,[1,2,4],'..'}] = tokenize(". ..").

dot_test() ->
  [{identifier,[1,0,3],foo},
   {'.',[1,3,4]},
   {identifier,[1,4,7],bar},
   {'.',[1,7,8]},
   {identifier,[1,8,11],baz}] = tokenize("foo.bar.baz").

dot_keyword_test() ->
 [{identifier,[1,0,3],foo},
  {'.',[1,3,4]},
  {identifier,[1,4,6],do}] = tokenize("foo.do").

newline_test() ->
 [{identifier,[1,0,3],foo},
  {'.',[2,0,1]},
  {identifier,[2,1,4],bar}]  = tokenize("foo\n.bar"),
  [{number,[1,0,1],1},
   {two_op,[2,0,2],'++'},
   {number,[2,2,3],2}]  = tokenize("1\n++2").

aliases_test() ->
  [{'aliases',[1,0,3],['Foo']}] = tokenize("Foo"),
  [{'aliases',[1,0,3],['Foo']},
   {'.',[1,3,4]},
   {'aliases',[1,4,7],['Bar']},
   {'.',[1,7,8]},
   {'aliases',[1,8,11],['Baz']}] = tokenize("Foo.Bar.Baz").

string_test() ->
  [{bin_string,[1,0,5],[<<"foo">>]}] = tokenize("\"foo\""),
  [{list_string,[1,0,5],[<<"foo">>]}] = tokenize("'foo'").

empty_string_test() ->
  [{bin_string,[1,0,2],[<<>>]}] = tokenize("\"\""),
  [{list_string,[1,0,2],[<<>>]}] = tokenize("''").

addadd_test() ->
  [{identifier,[1,0,1],x},{two_op,[1,2,4],'++'},{identifier,[1,5,6],y}] = tokenize("x ++ y").

space_test() ->
  [{op_identifier,[1,0,3],foo},{dual_op,[1,4,5],'-'},{number,[1,5,6],2}] = tokenize("foo -2"),
  [{op_identifier,[1,0,3],foo},{dual_op,[1,5,6],'-'},{number,[1,6,7],2}] = tokenize("foo  -2").

invalid_space_test() ->
  {1, "invalid space character U+A0 before: ", "-2"} = tokenize_error("foo" ++ [16#A0] ++"-2").

chars_test() ->
  [{number,[1,0,2],97}]      = tokenize("?a"),
  [{number,[1,0,2],99}]      = tokenize("?c"),
  [{number,[1,0,4],0}]       = tokenize("?\\0"),
  [{number,[1,0,4],7}]       = tokenize("?\\a"),
  [{number,[1,0,4],10}]      = tokenize("?\\n"),
  [{number,[1,0,5],92}]      = tokenize("?\\\\"),
  [{number,[1,0,5],10}]      = tokenize("?\\xa"),
  [{number,[1,0,7],10}]      = tokenize("?\\x{a}"),
  [{number,[1,0,8],171}]     = tokenize("?\\x{ab}"),
  [{number,[1,0,9],2748}]    = tokenize("?\\x{abc}"),
  [{number,[1,0,10],43981}]   = tokenize("?\\x{abcd}"),
  [{number,[1,0,11],703710}]  = tokenize("?\\x{abcde}"),
  [{number,[1,0,12],1092557}] = tokenize("?\\x{10abcd}").
