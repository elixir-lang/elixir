-module(tokenizer_test).
-include("elixir.hrl").
-include_lib("eunit/include/eunit.hrl").

tokenize(String) ->
  elixir_tokenizer:tokenize(String, 1).

integer_test() ->
  [{number, 1, 123}] = tokenize("123"),
  [{eol, 1}, {number, 3, 123}] = tokenize("\n\n123").
