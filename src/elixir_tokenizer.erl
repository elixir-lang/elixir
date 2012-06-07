-module(elixir_tokenizer).
-include("elixir.hrl").
-export([tokenize/2]).
-import(elixir_interpolation, [unescape_chars/1, unescape_tokens/1]).

-define(is_digit(S), S >= $0 andalso S =< $9).
-define(is_hex(S), ?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f)).
-define(is_bin(S), S >= $0 andalso S =< $1).
-define(is_octal(S), S >= $0 andalso S =< $7).
-define(is_upcase(S), S >= $A andalso S =< $Z).
-define(is_downcase(S), S >= $a andalso S =< $z).
-define(is_word(S), ?is_digit(S) orelse ?is_upcase(S) orelse ?is_downcase(S)).
-define(is_quote(S), S == $" orelse S == $').

-define(container2(T1, T2),
  T1 == ${, T2 == $};
  T1 == $[, T2 == $]
).

-define(comp3(T1, T2, T3),
  T1 == $=, T2 == $=, T3 == $=;
  T1 == $!, T2 == $=, T3 == $=
).

-define(comp2(T1, T2),
  T1 == $=, T2 == $=;
  T1 == $!, T2 == $=;
  T1 == $<, T2 == $=;
  T1 == $>, T2 == $=
).

-define(op2(T1, T2),
  T1 == $&, T2 == $&;
  T1 == $|, T2 == $|;
  T1 == $<, T2 == $>;
  T1 == $+, T2 == $+;
  T1 == $-, T2 == $-;
  T1 == $*, T2 == $*;
  T1 == $/, T2 == $/;
  T1 == $:, T2 == $:;
  T1 == $<, T2 == $-;
  T1 == $-, T2 == $>
).

-define(comp1(T),
  T == $<;
  T == $>
).

-define(op1(T),
  T == $+;
  T == $-;
  T == $*;
  T == $/;
  T == $=;
  T == $|;
  T == $!;
  T == $^;
  T == $@
).

tokenize(String, Line) ->
  tokenize(Line, String, []).

tokenize(_, [], Tokens) ->
  { ok, lists:reverse(Tokens) };

% Base integers

tokenize(Line, [$0,X,H|T], Tokens) when (X == $x orelse X == $X), ?is_hex(H) ->
  { Rest, Number } = tokenize_hex([H|T], []),
  tokenize(Line, Rest, [{number,Line,Number}|Tokens]);

tokenize(Line, [$0,O,H|T], Tokens) when (O == $o orelse O == $O), ?is_octal(H) ->
  { Rest, Number } = tokenize_octal([H|T], []),
  tokenize(Line, Rest, [{number,Line,Number}|Tokens]);

tokenize(Line, [$0,B,H|T], Tokens) when (B == $b orelse B == $B), ?is_bin(H) ->
  { Rest, Number } = tokenize_bin([H|T], []),
  tokenize(Line, Rest, [{number,Line,Number}|Tokens]);

% Comments

tokenize(Line, [$#|String], Tokens) ->
  Rest = tokenize_comment(String),
  tokenize(Line, Rest, Tokens);

% Sigils

tokenize(Line, [$%,S,H,H,H|T], Tokens) when ?is_quote(H), ?is_upcase(S) orelse ?is_downcase(S) ->
  case extract_heredoc_with_interpolation(Line, ?is_downcase(S), T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(Line, Final, [{sigil,Line,S,Parts,Modifiers}|Tokens])
  end;

tokenize(Line, [$%,S,H|T], Tokens) when not(?is_word(H)), ?is_upcase(S) orelse ?is_downcase(S) ->
  case elixir_interpolation:extract(Line, ?is_downcase(S), T, terminator(H)) of
    { NewLine, Parts, Rest } ->
      { Final, Modifiers } = collect_modifiers(Rest, []),
      tokenize(NewLine, Final, [{sigil,Line,S,Parts,Modifiers}|Tokens]);
    Else -> Else
  end;

% Char tokens

tokenize(Line, [$?,$\\,H|T], Tokens) ->
  Char = elixir_interpolation:unescape_map(H),
  tokenize(Line, T, [{number,Line,Char}|Tokens]);

tokenize(Line, [$?,Char|T], Tokens) ->
  tokenize(Line, T, [{number,Line,Char}|Tokens]);

% Dot identifier

tokenize(Line, "..." ++ Rest, Tokens) ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, '...', Rest)|Tokens]);

% Dot operators

% ## Containers
tokenize(Line, ".<<>>" ++ Rest, Tokens) ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, '<<>>', Rest),{'.',Line}|Tokens]);

tokenize(Line, [$.,T1,T2|Rest], Tokens) when ?container2(T1, T2) ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2]), Rest),{'.',Line}|Tokens]);

% ## Three Token Operators
tokenize(Line, [$.,T1,T2,T3|Rest], Tokens) when ?comp3(T1, T2, T3) ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2,T3]), Rest),{'.',Line}|Tokens]);

% ## Two Token Operators
tokenize(Line, [$.,T1,T2|Rest], Tokens) when ?comp2(T1, T2); ?op2(T1, T2) ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T1,T2]), Rest),{'.',Line}|Tokens]);

% ## Single Token Operators
tokenize(Line, [$.,T|Rest], Tokens) when ?comp1(T); ?op1(T); T == $& ->
  tokenize(Line, Rest, [tokenize_call_identifier(identifier, Line, list_to_atom([T]), Rest),{'.',Line}|Tokens]);

% Dot call

% ## Exception for .( as it needs to be treated specially in the parser
tokenize(Line, [$.,$(|Rest], Tokens) ->
  tokenize(Line, [$(|Rest], [{dot_call_op,Line,'.'}|Tokens]);

tokenize(Line, [$.,H|T], Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, true, T, H) of
    { NewLine, [Part], Rest } when is_binary(Part) ->
      Atom = binary_to_atom(Part, utf8),
      tokenize(NewLine, Rest, [tokenize_call_identifier(identifier, Line, Atom, Rest),{'.',Line}|Tokens]);
    Else -> Else
  end;

% Heredocs

tokenize(Line, "\"\"\"" ++ T, Tokens) -> handle_heredocs(Line, $", T, Tokens);
tokenize(Line, "'''" ++ T, Tokens)    -> handle_heredocs(Line, $', T, Tokens);

% Strings

tokenize(Line, [$"|T], Tokens) -> handle_strings(Line, $", T, Tokens);
tokenize(Line, [$'|T], Tokens) -> handle_strings(Line, $', T, Tokens);

% Atoms

tokenize(Line, [$:,T|String], Tokens) when ?is_upcase(T); ?is_downcase(T); T == $_ ->
  { Rest, Atom } = tokenize_atom([T|String], []),
  tokenize(Line, Rest, [{atom,Line,[Atom]}|Tokens]);

tokenize(Line, [$:,H|T], Tokens) when ?is_quote(H) ->
  case elixir_interpolation:extract(Line, true, T, H) of
    { NewLine, Parts, Rest } -> tokenize(NewLine, Rest, [{atom,Line,unescape_tokens(Parts)}|Tokens]);
    Else -> Else
  end;

% Atom operators

% ## Containers
tokenize(Line, ":<<>>" ++ Rest, Tokens) ->
  tokenize(Line, Rest, [{atom,Line,['<<>>']}|Tokens]);

tokenize(Line, [$:,T1,T2|Rest], Tokens) when ?container2(T1, T2) ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2])]}|Tokens]);

% ## Three Token Operators
tokenize(Line, [$:,T1,T2,T3|Rest], Tokens) when ?comp3(T1, T2, T3) ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2,T3])]}|Tokens]);

% ## Two Token Operators
tokenize(Line, [$:,T1,T2|Rest], Tokens) when ?comp2(T1, T2); ?op2(T1, T2) ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T1,T2])]}|Tokens]);

% ## Single Token Operators
tokenize(Line, [$:,T|Rest], Tokens) when ?comp1(T); ?op1(T); T == $&; T == $. ->
  tokenize(Line, Rest, [{atom,Line,[list_to_atom([T])]}|Tokens]);

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

% Stand-alone tokens

% ## &
tokenize(Line, [$&,H|Rest], Tokens) when ?is_digit(H) ->
  tokenize(Line, Rest, [{'&', Line, [list_to_integer([H])]}|Tokens]);

% ## Containers + punctuation tokens

tokenize(Line, [T,T|Rest], Tokens) when T == $<; T == $>; T == $. ->
  tokenize(Line, Rest, [{list_to_atom([T,T]), Line}|Tokens]);

tokenize(Line, [T|Rest], Tokens) when T == $(;
  T == ${; T == $}; T == $[; T == $]; T == $);
  T == $,; T == $. ->
  tokenize(Line, Rest, [{list_to_atom([T]), Line}|Tokens]);

% ## Comparison three token operators
tokenize(Line, [T1,T2,T3|Rest], Tokens) when ?comp3(T1, T2, T3) ->
  handle_comp_op(Line, list_to_atom([T1,T2,T3]), Rest, Tokens);

% ## Three token operators - none yet

% ## Comparison two token operators
tokenize(Line, [T1,T2|Rest], Tokens) when ?comp2(T1, T2) ->
  handle_comp_op(Line, list_to_atom([T1,T2]), Rest, Tokens);

% ## Two Token Operators
tokenize(Line, [T1,T2|Rest], Tokens) when ?op2(T1, T2) ->
  handle_op(Line, list_to_atom([T1,T2]), Rest, Tokens);

% ## Comparison single token operators
tokenize(Line, [T|Rest], Tokens) when ?comp1(T) ->
  handle_comp_op(Line, list_to_atom([T]), Rest, Tokens);

% ## Single Token Operators
tokenize(Line, [T|Rest], Tokens) when ?op1(T) ->
  handle_op(Line, list_to_atom([T]), Rest, Tokens);

% Integers and floats

tokenize(Line, [H|_] = String, Tokens) when ?is_digit(H) ->
  { Rest, Number } = tokenize_number(String, [], false),
  tokenize(Line, Rest, [{number,Line,Number}|Tokens]);

% Aliases

tokenize(Line, [H|_] = String, Tokens) when ?is_upcase(H) ->
  { Rest, Alias } = tokenize_identifier(String, [], false),
  tokenize(Line, Rest, [{'__aliases__',Line,[list_to_atom(Alias)]}|Tokens]);

% Identifier

tokenize(Line, [H|_] = String, Tokens) when ?is_downcase(H); H == $_ ->
  { Rest, { Kind, _, Identifier } } = tokenize_any_identifier(Line, String, []),
  case keyword(Line, Kind, Identifier) of
    false ->
      tokenize(Line, Rest, [{Kind,Line,Identifier}|Tokens]);
    Else  ->
      tokenize(Line, Rest, [Else|Tokens])
  end;

% Ambiguous unary/binary operators tokens

tokenize(Line, [Space,Sign,NotMarker|T], [{Identifier,_,_} = H|Tokens]) when Sign == $+ orelse Sign == $-,
  Space == $\s orelse Space == $\t, NotMarker /= $\s, NotMarker /= $\t,
  NotMarker /= $\n, NotMarker /= $:, NotMarker /= $(,
  NotMarker /= $+, NotMarker /= $-, NotMarker /= $>,
  Identifier == identifier orelse Identifier == punctuated_identifier ->
  Rest = [NotMarker|T],
  tokenize(Line, Rest, [{list_to_atom([Sign]),Line},setelement(1, H, op_identifier)|Tokens]);

% Spaces

tokenize(Line, [T|Rest], Tokens) when T == $ ; T == $\r; T == $\t ->
  tokenize(Line, Rest, Tokens);

tokenize(_, [{line,Line}|Rest], Tokens) ->
  tokenize(Line, Rest, Tokens);

tokenize(Line, T, _) ->
  { error, { Line, "invalid token: ", T } }.

%% Handlers

handle_heredocs(Line, H, T, Tokens) ->
  case extract_heredoc_with_interpolation(Line, true, T, H) of
    { error, _ } = Error ->
      Error;
    { Parts, Rest } ->
      Kind = case H == $" of
        true  -> bin_string;
        false -> list_string
      end,
      tokenize(Line, Rest, [{Kind,Line,unescape_tokens(Parts)}|Tokens])
  end.

handle_strings(Line, H, T, Tokens) ->
  case elixir_interpolation:extract(Line, true, T, H) of
    { NewLine, Parts, [$:|Rest] } ->
      case Parts of
        [Bin] when is_binary(Bin) ->
          Atom = binary_to_atom(unescape_chars(Bin), utf8),
          tokenize(NewLine, Rest, [{kw_identifier,Line,Atom}|Tokens]);
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
  end.

handle_comp_op(Line, Op, Rest, Tokens) ->
  tokenize(Line, Rest, [{comp_op, Line, Op}|Tokens]).

handle_op(Line, Op, Rest, Tokens) ->
  tokenize(Line, Rest, [{Op, Line}|Tokens]).

%% Helpers

eol(Line, Tokens) ->
  case Tokens of
    [{eol,_}|_] -> Tokens;
    _ -> [{eol,Line}|Tokens]
  end.

collect_modifiers([H|T], Buffer) when ?is_downcase(H) ->
  collect_modifiers(T, [H|Buffer]);

collect_modifiers(Rest, Buffer) ->
  { Rest, lists:reverse(Buffer) }.

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
  { Rest, list_to_float(lists:reverse(Acc)) };

% Or integer.
tokenize_number(Rest, Acc, false) ->
  { Rest, list_to_integer(lists:reverse(Acc)) }.

% Hex
tokenize_hex([H|T], Acc) when ?is_hex(H) -> tokenize_hex(T, [H|Acc]);
tokenize_hex(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 16) }.

% Octal
tokenize_octal([H|T], Acc) when ?is_octal(H) -> tokenize_octal(T, [H|Acc]);
tokenize_octal(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 8) }.

% Bin
tokenize_bin([H|T], Acc) when ?is_bin(H) -> tokenize_bin(T, [H|Acc]);
tokenize_bin(Rest, Acc) -> { Rest, list_to_integer(lists:reverse(Acc), 2) }.

% Comments

tokenize_comment("\r\n" ++ _ = Rest) -> Rest;
tokenize_comment("\n" ++ _ = Rest)   -> Rest;
tokenize_comment([_|Rest])           -> tokenize_comment(Rest);
tokenize_comment([])                 -> [].

% Identifiers

% Tokenize identifier. At this point, the validity of
% the first character was already verified.

tokenize_identifier([H|T], Acc, Marker) when Marker == atom, H == $@;
    ?is_digit(H); ?is_upcase(H); ?is_downcase(H); H == $_ ->
  tokenize_identifier(T, [H|Acc], Marker);

tokenize_identifier([], Acc, _Marker) ->
  { [], lists:reverse(Acc) };

tokenize_identifier(Rest, Acc, _Marker) ->
  { Rest, lists:reverse(Acc) }.

% Tokenize atom identifier, which also accepts punctuated identifiers
tokenize_atom(String, Acc) ->
  { Rest, Identifier } = tokenize_identifier(String, Acc, atom),
  case Rest of
    [H|T] when H == $?; H == $! ->
      { T, ?ELIXIR_ATOM_CONCAT([Identifier, [H]]) };
    _ ->
      { Rest, list_to_atom(Identifier) }
  end.

% Tokenize any identifier, handling kv, punctuated, paren, bracket and do identifiers.
tokenize_any_identifier(Line, String, Acc) ->
  { Rest, Identifier } = tokenize_identifier(String, Acc, false),
  case Rest of
    [H,$:|T] when H == $?; H == $! ->
      Atom = ?ELIXIR_ATOM_CONCAT([Identifier, [H]]),
      { T, { kw_identifier, Line, Atom } };
    [H|T] when H == $?; H == $! ->
      Atom = ?ELIXIR_ATOM_CONCAT([Identifier, [H]]),
      { T, tokenize_call_identifier(punctuated_identifier, Line, Atom, T) };
    [$:|T] ->
      Atom = list_to_atom(Identifier),
      { T, { kw_identifier, Line, Atom } };
    _ ->
      { Rest, tokenize_call_identifier(identifier, Line, list_to_atom(Identifier), Rest) }
  end.

tokenize_call_identifier(Kind, Line, Atom, Rest) ->
  case Rest of
    [$(|_] -> { paren_identifier, Line, Atom };
    [$[|_] -> { bracket_identifier, Line, Atom };
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

% Keywords check

keyword(Line, do_identifier, fn) ->
  { do_identifier, Line, fn };

keyword(Line, paren_identifier, fn) ->
  { 'fn_paren', Line };

keyword(Line, Identifier, Atom) when Identifier ==  identifier; Identifier == do_identifier ->
  case keyword(Atom) of
    true  -> { Atom, Line };
    false -> block_keyword(Atom) andalso { block_identifier, Line, Atom }
  end;

keyword(_, _, _) -> false.

% Keywords
keyword('fn')      -> true;
keyword('do')      -> true;
keyword('end')     -> true;
keyword('true')    -> true;
keyword('false')   -> true;
keyword('nil')     -> true;

% Operator keywords
keyword('not')     -> true;
keyword('and')     -> true;
keyword('or')      -> true;
keyword('xor')     -> true;
keyword('when')    -> true;
keyword('in')      -> true;
keyword('inlist')  -> true;
keyword('inbits')  -> true;
keyword(_)         -> false.

% Block keywords
block_keyword('after')  -> true;
block_keyword('else')   -> true;
block_keyword('rescue') -> true;
block_keyword('catch')  -> true;
block_keyword(_)        -> false.