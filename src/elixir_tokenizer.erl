-module(elixir_tokenizer).
-export([tokenize/2]).

tokenize(String, Line) ->
  tokenize(Line, String, []).

tokenize(_, [], Tokens) ->
  { ok, lists:reverse(Tokens) };

% Integers and floats

tokenize(Line, [H|_] = String, Tokens) when H >= 48 andalso H =< 57 ->
  { Rest, Token } = tokenize_number(Line, String, []),
  tokenize(Line, Rest, [Token|Tokens]);

% Operators/punctuation tokens

tokenize(Line, [T|Rest], Tokens) when T == $(; T == $); T == $,; T == $;; T == $+; T == $-; T == $*; T == $/ ->
  tokenize(Line, Rest, [{list_to_atom([T]), Line}|Tokens]);

% End of line

tokenize(Line, "\n" ++ Rest, Tokens) ->
  tokenize(Line + 1, Rest, eol(Line, Tokens));

tokenize(Line, "\r\n" ++ Rest, Tokens) ->
  tokenize(Line + 1, Rest, eol(Line, Tokens));

% Spaces

tokenize(Line, [T|Rest], Tokens) when T == $ ; T == $\r; T == $\t ->
  tokenize(Line, Rest, Tokens);

tokenize(Line, String, _) ->
  { error, { Line, "invalid token", String } }.

%% Helpers

eol(Line, Tokens) ->
  case Tokens of
    [{eol,_}|Rest] -> Tokens;
    _ -> [{eol,Line}|Tokens]
  end.

% Integers and floats

tokenize_number(Line, [$_,H|T], Acc) when H >= 48 andalso H =< 57 ->
  tokenize_number(Line, T, [H|Acc]);

tokenize_number(Line, [H|T], Acc) when H >= 48 andalso H =< 57 ->
  tokenize_number(Line, T, [H|Acc]);

tokenize_number(Line, Rest, Acc) ->
  { Rest, { number, Line, erlang:list_to_integer(lists:reverse(Acc)) } }.
