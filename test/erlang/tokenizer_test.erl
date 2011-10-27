-module(tokenizer_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  elixir_tokenizer:tokenize(String, 1).

integer_test() ->
  [{number, 1, 123}] = tokenize("123").