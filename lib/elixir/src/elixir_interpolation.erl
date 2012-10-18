% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/5, unescape_chars/1, unescape_chars/2,
unescape_tokens/1, unescape_tokens/2, unescape_map/1]).
-define(is_octal(S), S >= $0 andalso S =< $7).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Extract string interpolations

extract(Line, File, Interpol, String, Last) ->
  extract(Line, File, Interpol, String, [], [], [], Last).

extract(Line, File, _Interpol, [], Buffer, [], Output, []) ->
  finish_extraction(Line, File, Buffer, Output, []);

extract(Line, _File, _Interpol, [], _Buffer, [], _Output, Last) ->
  { error, { Line, io_lib:format("missing terminator: ~ts", [[Last]]), [] } };

extract(Line, File, _Interpol, [Last|Remaining], Buffer, [], Output, Last) ->
  finish_extraction(Line, File, Buffer, Output, Remaining);

extract(Line, _File, _Interpol, [Last], _Buffer, Search, _Output, Last) ->
  { error, { Line, io_lib:format("unexpected end of string: ~ts", [[hd(Search)]]), [Last] } };

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

%% Check for available separators "", {}, [] and () inside interpolation

extract(Line, File, Interpol, [C|Rest], Buffer, [C|Search], Output, Last) when C == $); C == $]; C == $}; C == $"; C == $' ->
  extract(Line, File, Interpol, Rest, [C|Buffer], Search, Output, Last);

extract(Line, File, Interpol, [C|Rest], Buffer, [_|_] = Search, Output, Last) when C == $"; C == $' ->
  extract(Line, File, Interpol, Rest, [C|Buffer], [C|Search], Output, Last);

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

-define(to_octal(List),

).

unescape_chars(String) ->
  unescape_chars(String, fun unescape_map/1).

unescape_chars(String, Map) ->
  Octals = case Map($0) of
    false -> false;
    _ -> true
  end,
  unescape_chars(String, Map, Octals, <<>>).

unescape_chars(<<$\\,A,B,C,Rest/binary>>, Map, true, Acc) when ?is_octal(A), ?is_octal(B), ?is_octal(C) ->
  to_octal(Rest, Map, [A,B,C], Acc);

unescape_chars(<<$\\,A,B,Rest/binary>>, Map, true, Acc) when ?is_octal(A), ?is_octal(B) ->
  to_octal(Rest, Map, [A,B], Acc);

unescape_chars(<<$\\,A,Rest/binary>>, Map, true, Acc) when ?is_octal(A) ->
  to_octal(Rest, Map, [A], Acc);

unescape_chars(<<$\\,Escaped,Rest/binary>>, Map, Octals, Acc) ->
  case Map(Escaped) of
    false -> unescape_chars(Rest, Map, Octals, <<Acc/binary, $\\, Escaped>>);
    Other -> unescape_chars(Rest, Map, Octals, <<Acc/binary, Other>>)
  end;

unescape_chars(<<Char, Rest/binary>>, Map, Octals, Acc) ->
  unescape_chars(Rest, Map, Octals, <<Acc/binary, Char>>);

unescape_chars(<<>>, _Map, _Octals, Acc) -> Acc.

to_octal(Rest, Map, Octal, Acc) ->
  unescape_chars(Rest, Map, true, <<Acc/binary, (list_to_integer(Octal, 8))/integer>>).

% Unescape Helpers

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

wrap_interpol(_Line, Form) when is_binary(Form) ->
  Form;

wrap_interpol(Line, Form) ->
  { '::', Line, [{ { '.', Line, ['Elixir.Binary.Chars', to_binary] }, Line, [Form]}, { binary, Line, nil }]}.

forms(String, StartLine, File) ->
  case elixir_translator:forms(String, StartLine, File, []) of
    { ok, [] } -> nil;
    { ok, [Forms] } when not is_list(Forms) -> Forms;
    { ok, Forms } -> { '__block__', StartLine, Forms };
    { error, Tuple } -> throw({ interpolation_error, Tuple })
  end.