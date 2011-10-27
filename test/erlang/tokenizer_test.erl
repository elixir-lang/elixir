-module(tokenizer_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  { ok, Result } = elixir_tokenizer:tokenize(String, 1),
  Result.

arithmetic_test() ->
  [{number,1,1},{'+',1},{number,1,2},{'+',1},{number,1,3}] = tokenize("1 + 2 + 3").

integer_test() ->
  [{number, 1, 123}] = tokenize("123"),
  [{number, 1, 123},{';', 1}] = tokenize("123;"),
  [{eol, 1}, {number, 3, 123}] = tokenize("\n\n123"),
  [{number, 1, 123}, {number, 1, 234}] = tokenize("  123  234  ").
