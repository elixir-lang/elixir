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

% Comments

tokenize(Line, [$#|String], Tokens) ->
  Rest = tokenize_comment(String),
  tokenize(Line, Rest, Tokens);

% Char tokens

tokenize(Line, [$?,$\\,H|T], Tokens) ->
  Chars = elixir_interpolation:unescape_chars(true, [$\\,H]),
  tokenize(Line, T, [{number,Line,lists:last(Chars)}|Tokens]);

tokenize(Line, [$?,H|T], Tokens) ->
  Chars = elixir_interpolation:unescape_chars(true, [H]),
  tokenize(Line, T, [{number,Line,lists:last(Chars)}|Tokens]);

% Dot operators

% ## Exception for .( as it needs to be treated specially in the parser
tokenize(Line, [$.,$(|Rest], Tokens) ->
  tokenize(Line, [$(|Rest], [{dot_call_op,Line,'.'}|Tokens]);

% ## Containers
tokenize(Line, [$.,T1,T2|Rest], Tokens) when T1 == $[ andalso T2 == $]; T1 == ${ andalso T2 == $} ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2]), Rest),{'.',Line}|Tokens]);

tokenize(Line, ".<<>>" ++ Rest, Tokens) ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, '<<>>', Rest),{'.',Line}|Tokens]);

% ## Three Token Operators
tokenize(Line, [$.,T1,T2,T3|Rest], Tokens) when
  T1 == $= andalso T2 == $= andalso T3 == $=;
  T1 == $! andalso T2 == $= andalso T3 == $= ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2,T3]), Rest),{'.',Line}|Tokens]);

% ## Two Token Operators
tokenize(Line, [$.,T1,T2|Rest], Tokens) when T1 == $& andalso T2 == $&;
  T1 == $| andalso T2 == $|; T1 == $: andalso T2 == $:;
  T1 == $= andalso T2 == $=; T1 == $! andalso T2 == $=;
  T1 == $< andalso T2 == $=; T1 == $> andalso T2 == $=;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/;
  T1 == $< andalso T2 == $- ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2]), Rest),{'.',Line}|Tokens]);

% ## Single Token Operators
tokenize(Line, [$.,T|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $&; T == $|; T == $!; T == $<; T == $>; T == $^; T == $@ ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T]), Rest),{'.',Line}|Tokens]);

% Strings

tokenize(Line, [H|T], Tokens) when H == $"; H == $' ->
  case elixir_interpolation:extract(Line, string, T, H) of
    { NewLine, Parts, [$:|Rest] } ->
      case Parts of
        [List] when is_list(List) ->
          tokenize(NewLine, Rest, [{kv_identifier,Line,list_to_atom(List)}|Tokens]);
        _ ->
          { error, { Line, "invalid interpolation in key", [$"|T] } }
      end;
    { NewLine, Parts, Rest } ->
      Kind = case H == $" of
        true  -> bin_string;
        false -> list_string
      end,
      tokenize(NewLine, Rest, [{Kind,Line,Parts}|Tokens]);
    Else -> Else
  end;

% Atoms

tokenize(Line, [$:,T|String], Tokens) when T >= $A andalso T =< $Z; T >= $a andalso T =< $z; T == $_ ->
  { Rest, { _, Atom } } = tokenize_identifier([T|String], []),
  tokenize(Line, Rest, [{atom,Line,[Atom]}|Tokens]);

tokenize(Line, [$:,H|T], Tokens) when H == $"; H == $' ->
  case elixir_interpolation:extract(Line, atom, T, H) of
    { NewLine, Parts, Rest } -> tokenize(NewLine, Rest, [{atom,Line,Parts}|Tokens]);
    Else -> Else
  end;

% Atom operators

% ## Containers
tokenize(Line, [$:,T1,T2|Rest], Tokens) when T1 == $[ andalso T2 == $]; T1 == ${ andalso T2 == $} ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2])]}|Tokens]);

tokenize(Line, ":<<>>" ++ Rest, Tokens) ->
  tokenize(Line, Rest, [{atom,Line,['<<>>']}|Tokens]);

% ## Three Token Operators
tokenize(Line, [$:,T1,T2,T3|Rest], Tokens) when
  T1 == $= andalso T2 == $= andalso T3 == $=;
  T1 == $! andalso T2 == $= andalso T3 == $= ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2,T3])]}|Tokens]);

% ## Two Token Operators
tokenize(Line, [$:,T1,T2|Rest], Tokens) when T1 == $& andalso T2 == $&;
  T1 == $| andalso T2 == $|; T1 == $: andalso T2 == $:;
  T1 == $= andalso T2 == $=; T1 == $! andalso T2 == $=;
  T1 == $< andalso T2 == $=; T1 == $> andalso T2 == $=;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/;
  T1 == $< andalso T2 == $- ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2])]}|Tokens]);

% ## Single Token Operators
tokenize(Line, [$:,T|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $&; T == $|; T == $!; T == $<; T == $>; T == $^; T == $@ ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T])]}|Tokens]);

% KV Identifiers

% ## Containers
tokenize(Line, [T1,T2,$:|Rest], Tokens) when T1 == $[ andalso T2 == $]; T1 == ${ andalso T2 == $} ->
  tokenize(Line, Rest, [{kv_identifier,Line,list_to_atom([T1,T2])}|Tokens]);

tokenize(Line, "<<>>:" ++ Rest, Tokens) ->
  tokenize(Line, Rest, [{kv_identifier,Line,'<<>>'}|Tokens]);

% ## Three Token Operators
tokenize(Line, [T1,T2,T3,$:|Rest], Tokens) when
  T1 == $= andalso T2 == $= andalso T3 == $=;
  T1 == $! andalso T2 == $= andalso T3 == $= ->
  tokenize(Line, Rest, [{kv_identifier,Line,list_to_atom([T1,T2,T3])}|Tokens]);

% ## Two Token Operators
tokenize(Line, [T1,T2,$:|Rest], Tokens) when T1 == $& andalso T2 == $&;
  T1 == $| andalso T2 == $|; T1 == $: andalso T2 == $:;
  T1 == $= andalso T2 == $=; T1 == $! andalso T2 == $=;
  T1 == $< andalso T2 == $=; T1 == $> andalso T2 == $=;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/;
  T1 == $< andalso T2 == $- ->
  tokenize(Line, Rest, [{kv_identifier,Line,list_to_atom([T1,T2])}|Tokens]);

% ## Single Token Operators
tokenize(Line, [T,$:|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $&; T == $|; T == $!; T == $<; T == $>; T == $^; T == $@ ->
  tokenize(Line, Rest, [{kv_identifier,Line,list_to_atom([T])}|Tokens]);

% Ambiguous unary/binary operators tokens

tokenize(Line, [Space,Sign,NotMarker|T], [{Identifier,_,_}|_] = Tokens) when Sign == $+ orelse Sign == $-,
  Space == $\s orelse Space == $\t, NotMarker /= $\s andalso NotMarker /= $\t andalso
  NotMarker /= $\n andalso NotMarker /= $: andalso NotMarker /= $( andalso NotMarker /= $+ andalso NotMarker /= $-,
  Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Line, Rest, [{special_op,Line,list_to_atom([Sign])}|Tokens]);

tokenize(Line, [Space,$:,$:,NotMarker|T], [{Identifier,_,_}|_] = Tokens) when Space == $\s orelse Space == $\t,
  NotMarker /= $\s andalso NotMarker /= $\t andalso
  NotMarker /= $\n andalso NotMarker /= $: andalso NotMarker /= $(,
  Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Line, Rest, [{special_op,Line,'::'}|Tokens]);

% Stand-alone tokens

% ## Containers + punctuation tokens
tokenize(Line, [T|Rest], Tokens) when T == $(;
  T == ${; T == $}; T == $[; T == $]; T == $);
  T == $,; T == $. ->
  tokenize(Line, Rest, [{list_to_atom([T]), Line}|Tokens]);

tokenize(Line, [T,T|Rest], Tokens) when T == $<; T == $> ->
  tokenize(Line, Rest, [{list_to_atom([T,T]), Line}|Tokens]);

% ## Comparison three token operators
tokenize(Line, [T1,T2,T3|Rest], Tokens) when
  T1 == $= andalso T2 == $= andalso T3 == $=;
  T1 == $! andalso T2 == $= andalso T3 == $= ->
  tokenize(Line, Rest, [{comp_op, Line, list_to_atom([T1,T2,T3])}|Tokens]);

% ## Three token operators - none yet

% ## Comparison two token operators
tokenize(Line, [T1,T2|Rest], Tokens) when
  T1 == $= andalso T2 == $=; T1 == $! andalso T2 == $=;
  T1 == $< andalso T2 == $=; T1 == $> andalso T2 == $=;
  T1 == $< andalso T2 == $- ->
  tokenize(Line, Rest, [{comp_op, Line, list_to_atom([T1,T2])}|Tokens]);

% ## Two Token Operators
tokenize(Line, [T1,T2|Rest], Tokens) when T1 == $& andalso T2 == $&;
  T1 == $| andalso T2 == $|; T1 == $: andalso T2 == $:;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/ ->
  tokenize(Line, Rest, [{list_to_atom([T1,T2]), Line}|Tokens]);

% ## Comparison single token operators
tokenize(Line, [T|Rest], Tokens) when T == $<; T == $> ->
  tokenize(Line, Rest, [{comp_op, Line, list_to_atom([T])}|Tokens]);

% ## Single Token Operators
tokenize(Line, [T|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $&; T == $|; T == $!; T == $^; T == $@ ->
  tokenize(Line, Rest, [{list_to_atom([T]), Line}|Tokens]);

% References

tokenize(Line, [H|_] = String, Tokens) when H >= $A andalso H =< $Z ->
  { Rest, { _, Identifier } } = tokenize_identifier(String, [], false),
  tokenize(Line, Rest, [{module_ref,Line,[Identifier]}|Tokens]);

% Identifier

tokenize(Line, [H|_] = String, Tokens) when H >= $a andalso H =< $z; H == $_ ->
  { Rest, { Kind, _, Identifier } } = tokenize_many_identifier(Line, String, []),
  HasKeyword = Kind == identifier orelse Kind == do_identifier orelse Kind == curly_identifier,
  case HasKeyword andalso keyword(Identifier) of
    true  ->
      tokenize(Line, Rest, [{Identifier,Line}|Tokens]);
    false ->
      tokenize(Line, Rest, [{Kind,Line,Identifier}|Tokens])
  end;

% End of line

tokenize(Line, ";" ++ Rest, []) ->
  tokenize(Line, Rest, eol(Line, []));

tokenize(Line, ";" ++ Rest, [Top|Tokens]) when element(1, Top) /= eol ->
  tokenize(Line, Rest, eol(Line, [Top|Tokens]));

tokenize(Line, "\\\n" ++ Rest, Tokens) ->
  tokenize(Line + 1, Rest, Tokens);

tokenize(Line, "\\\r\n" ++ Rest, Tokens) ->
  tokenize(Line + 1, Rest, Tokens);

tokenize(Line, "\n" ++ Rest, Tokens) ->
  tokenize(Line + 1, Rest, eol(Line, Tokens));

tokenize(Line, "\r\n" ++ Rest, Tokens) ->
  tokenize(Line + 1, Rest, eol(Line, Tokens));

% Spaces

tokenize(Line, [T|Rest], Tokens) when T == $ ; T == $\r; T == $\t ->
  tokenize(Line, Rest, Tokens);

tokenize(Line, T, _) ->
  { error, { Line, "invalid token: ", T } }.

%% Helpers

eol(Line, Tokens) ->
  case Tokens of
    [{eol,_}|_] -> Tokens;
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

% Check if we have e- followed by numbers. Valid only for floats.
tokenize_number([$e,S,H|T], Acc, true) when H >= $0 andalso H =< $9, S == $+ orelse S == $- ->
  tokenize_number(T, [H,S,$e|Acc], true);

% Check if we have e followed by numbers. Valid only for floats.
tokenize_number([$e,H|T], Acc, true) when H >= $0 andalso H =< $9 ->
  tokenize_number(T, [H,$e|Acc], true);

% Just numbers;
tokenize_number([H|T], Acc, Bool) when H >= $0 andalso H =< $9 ->
  tokenize_number(T, [H|Acc], Bool);

% Cast to float...
tokenize_number(Rest, Acc, true) ->
  { Rest, erlang:list_to_float(lists:reverse(Acc)) };

% Or integer.
tokenize_number(Rest, Acc, false) ->
  { Rest, erlang:list_to_integer(lists:reverse(Acc)) }.

% Comments

tokenize_comment("\r\n" ++ _ = Rest) -> Rest;
tokenize_comment("\n" ++ _ = Rest)   -> Rest;
tokenize_comment([_|Rest])       -> tokenize_comment(Rest);
tokenize_comment([])             -> [].

% Identifiers
% At this point, the validity of the first character was already verified.

tokenize_identifier(String, Acc) ->
  tokenize_identifier(String, Acc, true).

tokenize_identifier([H|T], Acc, Bool) when H >= $0 andalso H =< $9; H >= $A andalso H =< $Z; H >= $a andalso H =< $z; H == $_ ->
  tokenize_identifier(T, [H|Acc], Bool);

tokenize_identifier([H|Rest], Acc, true) when H == $?; H == $! ->
  { Rest, { punctuated_identifier, list_to_atom(lists:reverse([H|Acc])) } };

tokenize_identifier(Rest, Acc, _) ->
  { Rest, { identifier, list_to_atom(lists:reverse(Acc)) } }.

% Tokenize identifier checking if it is a kv_identifier or a call_identifier.
tokenize_many_identifier(Line, String, Acc) ->
  { Rest, { Kind, Atom } } = tokenize_identifier(String, Acc),
  case Rest of
    [$:|T] ->
      case T of
        [$:|_] -> { [$:|T], { identifier, Line, Atom } };
        _ -> { T, { kv_identifier, Line, Atom } }
      end;
    _ ->
      { Rest, tokenize_call_identifier(Kind, Line, Atom, Rest) }
  end.

% Tokenize identifier checking if it is a call_identifier.
tokenize_call_identifier(Kind, Line, Atom, Rest) ->
  case Rest of
    [$(|_] -> { paren_identifier, Line, Atom };
    _ ->
      case next_is_block(Rest) of
        []              -> { Kind, Line, Atom };
        BlockIdentifier -> { BlockIdentifier, Line, Atom }
      end
  end.

next_is_block([Space|Tokens]) when Space == $\t; Space == $\s ->
  next_is_block(Tokens);

next_is_block([$d,$o,H|_]) when
  H >= $0 andalso H =< $9; H >= $A andalso H =< $Z;
  H >= $a andalso H =< $z; H == $_; H == $: ->
  [];

next_is_block([$d,$o|_]) ->
  do_identifier;

next_is_block([${,$},$:|_]) ->
  [];

next_is_block([${|_]) ->
  curly_identifier;

next_is_block(_) ->
  [].

% Keywords
keyword('do')      -> true;
keyword('end')     -> true;
keyword('true')    -> true;
keyword('false')   -> true;
keyword('nil')     -> true;

% Keyword operators
keyword('not')     -> true;
keyword('and')     -> true;
keyword('or')      -> true;
keyword('xor')     -> true;
keyword('when')    -> true;
keyword('in')      -> true;
keyword(_)         -> false.