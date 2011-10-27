-module(elixir_tokenizer).
-export([tokenize/2]).

tokenize(String, Line) ->
  tokenize(Line, String, []).

tokenize(_, [], Tokens) ->
  lists:reverse(Tokens);

tokenize(Line, [T|Rest], Tokens) when T == $(; T == $); T == $,; T == $; ->
  tokenize(Line, Rest, [{T, Line}|Tokens]);

tokenize(Line, [H|_] = String, Tokens) when H >= 48 andalso H =< 57 ->
  { Rest, Token } = tokenize_number(Line, String, []),
  tokenize(Line, Rest, [Token|Tokens]).

% Integers and floats

tokenize_number(Line, [H|T], Acc) when H >= 48 andalso H =< 57 ->
  tokenize_number(Line, T, [H|Acc]);

tokenize_number(Line, Rest, Acc) ->
  { Rest, { number, Line, erlang:list_to_integer(lists:reverse(Acc)) } }.
