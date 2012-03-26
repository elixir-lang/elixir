-module(elixir_tokenizer).
-export([tokenize/2]).
-import(elixir_interpolation, [unescape_chars/1, unescape_tokens/1]).

-define(is_digit(S), S >= $0 andalso S =< $9).
-define(is_upcase(S), S >= $A andalso S =< $Z).
-define(is_downcase(S), S >= $a andalso S =< $z).

tokenize(String, Line) ->
  tokenize(Line, String, []).

tokenize(_, [], Tokens) ->
  { ok, lists:reverse(Tokens) };

% Integers and floats

tokenize(Line, [H|_] = String, Tokens) when ?is_digit(H) ->
  { Rest, Number } = tokenize_number(String, [], false),
  tokenize(Line, Rest, [{number,Line,Number}|Tokens]);

% Comments

tokenize(Line, [$#|String], Tokens) ->
  Rest = tokenize_comment(String),
  tokenize(Line, Rest, Tokens);

% Sigils

tokenize(Line, [$%,S,H,H,H|T], Tokens) when H == $" orelse H == $', ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, ?is_downcase(S), T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      tokenize(Line, Rest, [{sigil,Line,S,Parts}|Tokens])
  end;

tokenize(Line, [$%,S,H|T], Tokens) when ?is_upcase(S); ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, ?is_downcase(S), T, terminator(H)) of
    { NewLine, Parts, Rest } ->
      tokenize(NewLine, Rest, [{sigil,Line,S,Parts}|Tokens]);
    Else -> Else
  end;

% Char tokens

tokenize(Line, [$?,$\\,H|T], Tokens) ->
  Chars = elixir_interpolation:unescape_chars([$\\,H]),
  tokenize(Line, T, [{number,Line,lists:last(Chars)}|Tokens]);

tokenize(Line, [$?,H|T], Tokens) ->
  Chars = elixir_interpolation:unescape_chars([H]),
  tokenize(Line, T, [{number,Line,lists:last(Chars)}|Tokens]);

% Stab

tokenize(Line, "->" ++ Rest, Tokens) ->
  tokenize(Line, Rest, [{'->',Line}|Tokens]);

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
  T1 == $| andalso T2 == $|; T1 == $< andalso T2 == $>;
  T1 == $= andalso T2 == $=; T1 == $! andalso T2 == $=;
  T1 == $< andalso T2 == $=; T1 == $> andalso T2 == $=;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/;
  T1 == $< andalso T2 == $- ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2]), Rest),{'.',Line}|Tokens]);

% ## Single Token Operators
tokenize(Line, [$.,T|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $|; T == $!; T == $<; T == $>; T == $^; T == $@ ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T]), Rest),{'.',Line}|Tokens]);

% Heredocs

tokenize(Line, [H,H,H|T], Tokens) when H == $"; H == $' ->
  case extract_heredoc_with_interpolation(Line, true, T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      Kind = case H == $" of
        true  -> bin_string;
        false -> list_string
      end,
      tokenize(Line, Rest, [{Kind,Line,unescape_tokens(Parts)}|Tokens])
  end;

% Strings

tokenize(Line, [H|T], Tokens) when H == $"; H == $' ->
  case elixir_interpolation:extract(Line, true, T, H) of
    { NewLine, Parts, [$:|Rest] } ->
      case Parts of
        [List] when is_list(List) ->
          tokenize(NewLine, Rest, [{kv_identifier,Line,list_to_atom(unescape_chars(List))}|Tokens]);
        _ ->
          { error, { Line, "invalid interpolation in key", [$"|T] } }
      end;
    { NewLine, Parts, Rest } ->
      Kind = case H == $" of
        true  -> bin_string;
        false -> list_string
      end,
      tokenize(NewLine, Rest, [{Kind,Line,unescape_tokens(Parts)}|Tokens]);
    Else -> Else
  end;

% Atoms

tokenize(Line, [$:,T|String], Tokens) when ?is_upcase(T); ?is_downcase(T); T == $_ ->
  { Rest, { _, Atom } } = tokenize_identifier([T|String], []),
  tokenize(Line, Rest, [{atom,Line,[Atom]}|Tokens]);

tokenize(Line, [$:,H|T], Tokens) when H == $"; H == $' ->
  case elixir_interpolation:extract(Line, true, T, H) of
    { NewLine, Parts, Rest } -> tokenize(NewLine, Rest, [{atom,Line,unescape_tokens(Parts)}|Tokens]);
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
  T1 == $| andalso T2 == $|; T1 == $< andalso T2 == $>;
  T1 == $= andalso T2 == $=; T1 == $! andalso T2 == $=;
  T1 == $< andalso T2 == $=; T1 == $> andalso T2 == $=;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/;
  T1 == $< andalso T2 == $- ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2])]}|Tokens]);

% ## Single Token Operators
tokenize(Line, [$:,T|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $|; T == $!; T == $<; T == $>; T == $^; T == $@ ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T])]}|Tokens]);

% Ambiguous unary/binary operators tokens

tokenize(Line, [Space,Sign,NotMarker|T], [{Identifier,_,_}|_] = Tokens) when Sign == $+ orelse Sign == $-,
  Space == $\s orelse Space == $\t, NotMarker /= $\s, NotMarker /= $\t,
  NotMarker /= $\n, NotMarker /= $:, NotMarker /= $(,
  NotMarker /= $+, NotMarker /= $-, NotMarker /= $>,
  Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Line, Rest, [{special_op,Line,list_to_atom([Sign])}|Tokens]);

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
  T1 == $| andalso T2 == $|; T1 == $< andalso T2 == $>;
  T1 == $+ andalso T2 == $+; T1 == $- andalso T2 == $-;
  T1 == $* andalso T2 == $*; T1 == $/ andalso T2 == $/ ->
  tokenize(Line, Rest, [{list_to_atom([T1,T2]), Line}|Tokens]);

% ## Comparison single token operators
tokenize(Line, [T|Rest], Tokens) when T == $<; T == $> ->
  tokenize(Line, Rest, [{comp_op, Line, list_to_atom([T])}|Tokens]);

% ## Single Token Operators

tokenize(Line, [T|Rest], Tokens) when T == $+; T == $-; T == $*;
  T == $/; T == $=; T == $|; T == $!; T == $^; T == $@ ->
  tokenize(Line, Rest, [{list_to_atom([T]), Line}|Tokens]);

% &

tokenize(Line, [$&,H|Rest], Tokens) when ?is_digit(H) ->
  tokenize(Line, Rest, [{'&', Line, [list_to_integer([H])]}|Tokens]);

% References

tokenize(Line, [H|_] = String, Tokens) when ?is_upcase(H) ->
  { Rest, { _, Identifier } } = tokenize_identifier(String, [], false),
  tokenize(Line, Rest, [{'__ref__',Line,[Identifier]}|Tokens]);

% Identifier

tokenize(Line, [H|_] = String, Tokens) when ?is_downcase(H); H == $_ ->
  { Rest, { Kind, _, Identifier } } = tokenize_many_identifier(Line, String, []),
  HasKeyword = Kind == identifier orelse Kind == do_identifier,
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

tokenize(_, [{line,Line}|Rest], Tokens) ->
  tokenize(Line, Rest, Tokens);

tokenize(Line, T, _) ->
  { error, { Line, "invalid token: ", T } }.

%% Helpers

eol(Line, Tokens) ->
  case Tokens of
    [{eol,_}|_] -> Tokens;
    _ -> [{eol,Line}|Tokens]
  end.

% Extract heredocs

extract_heredoc_with_interpolation(Line, Interpol, T, H) ->
  case extract_heredoc(Line, T, H) of
    { error, _ } = Error ->
      Error;
    { Body, Rest } ->
      case elixir_interpolation:extract(Line, Interpol, Body, 0) of
        { _, Parts, [] } -> { Parts, Rest };
        Else -> Else
      end
  end.

extract_heredoc(Line0, Rest0, Marker) ->
  case extract_heredoc_line(Rest0, []) of
    { ok, Extra, Rest1 } ->
      %% We prepend a new line so we can transparently remove
      %% spaces later. This new line is removed by calling `tl`
      %% in the final heredoc body three lines below.
      case extract_heredoc_body(Line0, Marker, [$\n|Rest1], []) of
        { Line1, Body, Rest2, Spaces } ->
          { tl(remove_heredoc_spaces(Body, Spaces)), merge_heredoc_extra(Line1, Extra, Rest2) };
        { error, _ } = Reason ->
          Reason
      end;
    { error, eof } ->
      { error, { Line0, "unexpected end of file. invalid token: ", "EOF" } }
  end.

%% Remove spaces from heredoc based on the position of the final quotes.

remove_heredoc_spaces(Body, 0) ->
  lists:reverse([0|Body]);

remove_heredoc_spaces(Body, Spaces) ->
  remove_heredoc_spaces([0|Body], [], Spaces, Spaces).

remove_heredoc_spaces([H,$\n|T], [Backtrack|Buffer], Spaces, Original) when Spaces > 0, H == $\s orelse H == $\t ->
  remove_heredoc_spaces([Backtrack,$\n|T], Buffer, Spaces - 1, Original);

remove_heredoc_spaces([$\n=H|T], Buffer, _Spaces, Original) ->
  remove_heredoc_spaces(T, [H|Buffer], Original, Original);

remove_heredoc_spaces([H|T], Buffer, Spaces, Original) ->
  remove_heredoc_spaces(T, [H|Buffer], Spaces, Original);

remove_heredoc_spaces([], Buffer, _Spaces, _Original) ->
  Buffer.

%% Extract heredoc body. It returns the heredoc body (in reverse order),
%% the remaining of the document and the number of spaces the heredoc
%% is aligned.

extract_heredoc_body(Line, Marker, Rest, Buffer) ->
  case extract_heredoc_line(Marker, Rest, Buffer, 0) of
    { ok, NewBuffer, NewRest } ->
      extract_heredoc_body(Line + 1, Marker, NewRest, NewBuffer);
    { ok, NewBuffer, NewRest, Spaces } ->
      { Line + 1, NewBuffer, NewRest, Spaces };
    { error, eof } ->
      { error, { Line, "unexpected end of file. invalid token: ", "EOF" } }
  end.

%% Extract a line from the heredoc prepending its contents to a buffer.

extract_heredoc_line("\r\n" ++ Rest, Buffer) ->
  { ok, [$\n|Buffer], Rest };

extract_heredoc_line("\n" ++ Rest, Buffer) ->
  { ok, [$\n|Buffer], Rest };

extract_heredoc_line([H|T], Buffer) ->
  extract_heredoc_line(T, [H|Buffer]);

extract_heredoc_line(_, _) ->
  { error, eof }.

%% Extract each heredoc line trying to find a match according to the marker.

extract_heredoc_line(Marker, [H|T], Buffer, Counter) when H == $\t; H == $\s ->
  extract_heredoc_line(Marker, T, [H|Buffer], Counter + 1);

extract_heredoc_line(Marker, [Marker,Marker,Marker,$\n|T], Buffer, Counter) ->
  { ok, Buffer, T, Counter };

extract_heredoc_line(Marker, [Marker,Marker,Marker,$\r,$\n|T], Buffer, Counter) ->
  { ok, Buffer, T, Counter };

extract_heredoc_line(_Marker, Rest, Buffer, _Counter) ->
  extract_heredoc_line(Rest, Buffer).

%% Merge heredoc extra by replying it on the buffer. It also adds
%% a special { line, Line } token to force a line change along the way.

merge_heredoc_extra(Line, Extra, Buffer) ->
  merge_heredoc_extra(Extra, [{line,Line}|Buffer]).

merge_heredoc_extra([H|T], Buffer) ->
  merge_heredoc_extra(T, [H|Buffer]);

merge_heredoc_extra([], Buffer) ->
  Buffer.

% Integers and floats
% At this point, we are at least sure the first digit is a number.

% Check if we have a point followed by a number;
tokenize_number([$.,H|T], Acc, false) when ?is_digit(H) ->
  tokenize_number(T, [H,$.|Acc], true);

% Check if we have an underscore followed by a number;
tokenize_number([$_,H|T], Acc, Bool) when ?is_digit(H) ->
  tokenize_number(T, [H|Acc], Bool);

% Check if we have e- followed by numbers. Valid only for floats.
tokenize_number([$e,S,H|T], Acc, true) when ?is_digit(H), S == $+ orelse S == $- ->
  tokenize_number(T, [H,S,$e|Acc], true);

% Check if we have e followed by numbers. Valid only for floats.
tokenize_number([$e,H|T], Acc, true) when ?is_digit(H) ->
  tokenize_number(T, [H,$e|Acc], true);

% Just numbers;
tokenize_number([H|T], Acc, Bool) when ?is_digit(H) ->
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
tokenize_comment([_|Rest])           -> tokenize_comment(Rest);
tokenize_comment([])                 -> [].

% Identifiers
% At this point, the validity of the first character was already verified.

tokenize_identifier(String, Acc) ->
  tokenize_identifier(String, Acc, true).

tokenize_identifier([H|T], Acc, Bool) when ?is_digit(H); ?is_upcase(H); ?is_downcase(H); H == $_ ->
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
  ?is_digit(H); ?is_upcase(H); ?is_downcase(H); H == $_; H == $: ->
  [];

next_is_block([$d,$o|_]) ->
  do_identifier;

next_is_block(_) ->
  [].

% Terminator
terminator($() -> $);
terminator($[) -> $];
terminator(${) -> $};
terminator($<) -> $>;
terminator(O) -> O.

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