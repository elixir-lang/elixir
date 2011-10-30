-module(elixir_tokenizer).
-export([tokenize/2]).

tokenize(String, Line) ->
  tokenize(Line, String, []).

tokenize(_, [], Tokens) ->
  { ok, lists:reverse(Tokens) };

% Integers and floats

tokenize(Line, [H|_] = String, Tokens) when H >= $0 andalso H =< $9 ->
  { Rest, Number } = tokenize_number(String, [], false),
  tokenize(Line, Rest, [{number,Line,Number}|Tokens]);

% Atoms

tokenize(Line, [$:,T|String], Tokens) when T >= $A andalso T =< $Z; T >= $a andalso T =< $z; T == $_ ->
  { Rest, { _, Atom } } = tokenize_identifier([T|String], []),
  tokenize(Line, Rest, [{atom,Line,Atom}|Tokens]);

% Atom operators

tokenize(Line, [$:,T|Rest], Tokens) when T == $+; T == $-; T == $*; T == $/; T == $= ->
  tokenize(Line, Rest, [{atom,Line,list_to_atom([T])}|Tokens]);

% Call operators

tokenize(Line, [T,$(|Rest], Tokens) when T == $+; T == $-; T == $*; T == $/; T == $= ->
  tokenize(Line, [$(|Rest], [{call_op,Line,list_to_atom([T])}|Tokens]);

% Operators/punctuation tokens

tokenize(Line, [T|Rest], Tokens) when T == $(; T == $); T == $,;
  T == $;; T == $+; T == $-; T == $*; T == $/; T == $= ->
  tokenize(Line, Rest, [{list_to_atom([T]), Line}|Tokens]);

% Identifier

tokenize(Line, [T|String], Tokens) when T >= $a andalso T =< $z; T == $_ ->
  { Rest, { Kind, Identifier } } = tokenize_paren_identifier([T|String], []),
  tokenize(Line, Rest, [{Kind,Line,Identifier}|Tokens]);

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
% At this point, we are at least sure the first digit is a number.

% Check if we have a point followed by a number;
tokenize_number([$.,H|T], Acc, false) when H >= $0 andalso H =< $9 ->
  tokenize_number(T, [H,$.|Acc], true);

% Check if we have an underscore followed by a number;
tokenize_number([$_,H|T], Acc, Bool) when H >= $0 andalso H =< $9 ->
  tokenize_number(T, [H|Acc], Bool);

% Just numbers;
tokenize_number([H|T], Acc, Bool) when H >= $0 andalso H =< $9 ->
  tokenize_number(T, [H|Acc], Bool);

% Cast to float...
tokenize_number(Rest, Acc, true) ->
  { Rest, erlang:list_to_float(lists:reverse(Acc)) };

% Or integer.
tokenize_number(Rest, Acc, false) ->
  { Rest, erlang:list_to_integer(lists:reverse(Acc)) }.

% Identifiers
% At this point, the validity of the first character was already verified.

tokenize_identifier([H|T], Acc) when H >= $0 andalso H =< $9; H >= $A andalso H =< $Z; H >= $a andalso H =< $z; H == $_ ->
  tokenize_identifier(T, [H|Acc]);

tokenize_identifier([H|Rest], Acc) when H == $?; H =< $! ->
  { Rest, { punctuation_identifier, list_to_atom(lists:reverse([H|Acc])) } };

tokenize_identifier(Rest, Acc) ->
  { Rest, { identifier, list_to_atom(lists:reverse(Acc)) } }.

tokenize_paren_identifier(String, Acc) ->
  { Rest, { Kind, Atom } } = tokenize_identifier(String, Acc),
  case Rest of
    [$(|_] -> { Rest, { paren_identifier, Atom } };
    _ -> { Rest, { Kind, Atom } }
  end.