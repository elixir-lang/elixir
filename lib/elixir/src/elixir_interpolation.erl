% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/5, unescape_chars/1, unescape_chars/2,
unescape_tokens/1, unescape_tokens/2, unescape_map/1]).
-include("elixir.hrl").

%% Extract string interpolations

extract(Line, File, Interpol, String, Last) ->
  extract(Line, File, Interpol, String, [], [], [], Last).

extract(Line, File, _Interpol, [], Buffer, [], Output, []) ->
  finish_extraction(Line, File, Buffer, Output, []);

extract(Line, _File, _Interpol, [], _Buffer, [], _Output, Last) ->
  { error, { Line, io_lib:format("missing terminator: ~ts", [[Last]]), [] } };

extract(Line, File, _Interpol, [Last|Remaining], Buffer, [], Output, Last) ->
  finish_extraction(Line, File, Buffer, Output, Remaining);

extract(Line, _File, _Interpol, End, _Buffer, _Search, _Output, Last) when End == [Last]; End == [] ->
  { error, { Line, io_lib:format("missing terminator: ~ts", [[Last]]), [] } };

extract(Line, File, Interpol, [$\n|Rest], Buffer, Search, Output, Last) ->
  extract(Line+1, File, Interpol, Rest, [$\n|Buffer], Search, Output, Last);

extract(Line, File, Interpol, [$\\, $#, ${|Rest], Buffer, [], Output, Last) ->
  extract(Line, File, Interpol, Rest, [${,$#|Buffer], [], Output, Last);

extract(Line, File, Interpol, [$\\,Char|Rest], Buffer, [], Output, Last) ->
  extract(Line, File, Interpol, Rest, [Char,$\\|Buffer], [], Output, Last);

extract(Line, File, true, [$#, ${|Rest], Buffer, [], Output, Last) ->
  NewOutput = build_interpol(s, Line, File, Buffer, Output),
  extract(Line, File, true, Rest, [], [$}], NewOutput, Last);

extract(Line, File, true, [$}|Rest], Buffer, [$}], Output, Last) ->
  NewOutput = build_interpol(i, Line, File, Buffer, Output),
  extract(Line, File, true, Rest, [], [], NewOutput, Last);

%% Check for available separators inside interpolation

extract(Line, File, Interpol, [C|Rest], Buffer, [C|Search], Output, Last) when C == $); C == $]; C == $}; C == $>; C == $"; C == $' ->
  extract(Line, File, Interpol, Rest, [C|Buffer], Search, Output, Last);

extract(Line, File, Interpol, [C|Rest], Buffer, [_|_] = Search, Output, Last) when C == $"; C == $' ->
  extract(Line, File, Interpol, Rest, [C|Buffer], [C|Search], Output, Last);

extract(Line, File, Interpol, [$<|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, File, Interpol, Rest, [$<|Buffer], [$>|Search], Output, Last);

extract(Line, File, Interpol, [${|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, File, Interpol, Rest, [${|Buffer], [$}|Search], Output, Last);

extract(Line, File, Interpol, [$[|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, File, Interpol, Rest, [$[|Buffer], [$]|Search], Output, Last);

extract(Line, File, Interpol, [$(|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, File, Interpol, Rest, [$(|Buffer], [$)|Search], Output, Last);

%% Else

extract(Line, File, Interpol, [Char|Rest], Buffer, Search, Output, Last) ->
  extract(Line, File, Interpol, Rest, [Char|Buffer], Search, Output, Last).

%% Unescape a series of tokens as returned by extract.

unescape_tokens(Tokens) ->
  unescape_tokens(Tokens, fun unescape_map/1).

unescape_tokens(Tokens, Map) ->
  [unescape_token(Token, Map) || Token <- Tokens].

unescape_token(Token, Map) when is_binary(Token) -> unescape_chars(Token, Map);
unescape_token(Other, _Map) -> Other.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) ->
  unescape_chars(String, fun unescape_map/1).

unescape_chars(String, Map) ->
  Octals = Map($0) /= false,
  Hex    = Map($x) /= false,
  unescape_chars(String, Map, Octals, Hex, <<>>).

unescape_chars(<<$\\,A,B,C,Rest/binary>>, Map, true, Hex, Acc) when ?is_octal(A), A =< $3, ?is_octal(B), ?is_octal(C) ->
  append_escaped(Rest, Map, [A,B,C], true, Hex, Acc, 8);

unescape_chars(<<$\\,A,B,Rest/binary>>, Map, true, Hex, Acc) when ?is_octal(A), ?is_octal(B) ->
  append_escaped(Rest, Map, [A,B], true, Hex, Acc, 8);

unescape_chars(<<$\\,A,Rest/binary>>, Map, true, Hex, Acc) when ?is_octal(A) ->
  append_escaped(Rest, Map, [A], true, Hex, Acc, 8);

unescape_chars(<<$\\,P,A,B,Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B) ->
  append_escaped(Rest, Map, [A,B], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,A,Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A) ->
  append_escaped(Rest, Map, [A], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,${,A,$},Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A) ->
  append_escaped(Rest, Map, [A], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,${,A,B,$},Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B) ->
  append_escaped(Rest, Map, [A,B], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,${,A,B,C,$},Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  append_escaped(Rest, Map, [A,B,C], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,${,A,B,C,D,$},Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  append_escaped(Rest, Map, [A,B,C,D], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,${,A,B,C,D,E,$},Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
    append_escaped(Rest, Map, [A,B,C,D,E], Octal, true, Acc, 16);

unescape_chars(<<$\\,P,${,A,B,C,D,E,F,$},Rest/binary>>, Map, Octal, true, Acc) when (P == $x orelse P == $X), ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  append_escaped(Rest, Map, [A,B,C,D,E,F], Octal, true, Acc, 16);

unescape_chars(<<$\\,Escaped,Rest/binary>>, Map, Octals, Hex, Acc) ->
  case Map(Escaped) of
    false -> unescape_chars(Rest, Map, Octals, Hex, <<Acc/binary, $\\, Escaped>>);
    Other -> unescape_chars(Rest, Map, Octals, Hex, <<Acc/binary, Other>>)
  end;

unescape_chars(<<Char, Rest/binary>>, Map, Octals, Hex, Acc) ->
  unescape_chars(Rest, Map, Octals, Hex, <<Acc/binary, Char>>);

unescape_chars(<<>>, _Map, _Octals, _Hex, Acc) -> Acc.

append_escaped(Rest, Map, List, Octal, Hex, Acc, Base) ->
  Codepoint = list_to_integer(List, Base),
  unescape_chars(Rest, Map, Octal, Hex, <<Acc/binary, Codepoint/utf8>>).

% Unescape Helpers

unescape_map($a) -> 7;
unescape_map($b) -> $\b;
unescape_map($d) -> $\d;
unescape_map($e) -> $\e;
unescape_map($f) -> $\f;
unescape_map($n) -> $\n;
unescape_map($r) -> $\r;
unescape_map($s) -> $\s;
unescape_map($t) -> $\t;
unescape_map($v) -> $\v;
unescape_map(E)  -> E.

% Extract Helpers

finish_extraction(Line, File, Buffer, Output, Remaining) ->
  case build_interpol(s, Line, File, Buffer, Output) of
    []    -> Final = [<<>>];
    Final -> []
  end,
  { Line, lists:reverse(Final), Remaining }.

build_interpol(_Kind, _Line, _File, [], Output) ->
  Output;

build_interpol(s, _Line, _File, Buffer, Output) ->
  [unicode:characters_to_binary(lists:reverse(Buffer))|Output];

build_interpol(i, Line, File, Buffer, Output) ->
  [wrap_interpol(Line, forms(lists:reverse(Buffer), Line, File))| Output].

wrap_interpol(Line, Form) ->
  Meta = [{line,Line}],
  { '::', Meta, [{ { '.', Meta, ['Elixir.Kernel', to_binary] }, Meta, [Form]}, { binary, Meta, nil }]}.

forms(String, StartLine, File) ->
  case elixir_translator:forms(String, StartLine, File, []) of
    { ok, [] } -> nil;
    { ok, [Forms] } when not is_list(Forms) -> Forms;
    { ok, Forms } -> { '__block__', [{line,StartLine}], Forms };
    { error, Tuple } -> throw({ interpolation_error, Tuple })
  end.
