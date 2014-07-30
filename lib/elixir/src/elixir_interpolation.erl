% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/5, unescape_chars/1, unescape_chars/2,
unescape_tokens/1, unescape_tokens/2, unescape_map/1]).
-include("elixir.hrl").

%% Extract string interpolations

extract(Line, Raw, Interpol, String, Last) ->
  %% Ignore whatever is in the scope and enable terminator checking.
  Scope = Raw#elixir_tokenizer{terminators=[], check_terminators=true},
  extract(Line, Scope, Interpol, String, [], [], Last).

%% Terminators

extract(Line, _Scope, _Interpol, [], Buffer, Output, []) ->
  finish_extraction(Line, Buffer, Output, []);

extract(Line, _Scope, _Interpol, [], _Buffer, _Output, Last) ->
  {error, {string, Line, io_lib:format("missing terminator: ~ts", [[Last]]), []}};

extract(Line, _Scope, _Interpol, [Last|Remaining], Buffer, Output, Last) ->
  finish_extraction(Line, Buffer, Output, Remaining);

%% Going through the string

extract(Line, Scope, Interpol, [$\\, $\n|Rest], Buffer, Output, Last) ->
  extract(Line+1, Scope, Interpol, Rest, Buffer, Output, Last);

extract(Line, Scope, Interpol, [$\\, $\r, $\n|Rest], Buffer, Output, Last) ->
  extract(Line+1, Scope, Interpol, Rest, Buffer, Output, Last);

extract(Line, Scope, Interpol, [$\n|Rest], Buffer, Output, Last) ->
  extract(Line+1, Scope, Interpol, Rest, [$\n|Buffer], Output, Last);

extract(Line, Scope, true, [$\\, $#, ${|Rest], Buffer, Output, Last) ->
  extract(Line, Scope, true, Rest, [${,$#|Buffer], Output, Last);

extract(Line, Scope, true, [$#, ${|Rest], Buffer, Output, Last) ->
  Output1 = build_string(Line, Buffer, Output),

  case elixir_tokenizer:tokenize(Rest, Line, Scope) of
    {error, {EndLine, _, "}"}, [$}|NewRest], Tokens} ->
      Output2 = build_interpol(Line, Tokens, Output1),
      extract(EndLine, Scope, true, NewRest, [], Output2, Last);
    {error, Reason, _, _} ->
      {error, Reason};
    {ok, _EndLine, _} ->
      {error, {string, Line, "missing interpolation terminator:}", []}}
  end;

extract(Line, Scope, Interpol, [$\\,Char|Rest], Buffer, Output, Last) ->
  extract(Line, Scope, Interpol, Rest, [Char,$\\|Buffer], Output, Last);

%% Catch all clause

extract(Line, Scope, Interpol, [Char|Rest], Buffer, Output, Last) ->
  extract(Line, Scope, Interpol, Rest, [Char|Buffer], Output, Last).

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
  Hex    = Map($x) == true,
  unescape_chars(String, Map, Octals, Hex, <<>>).

%% Deprecated

unescape_chars(<<$\\,A,B,C,Rest/binary>>, Map, true, Hex, Acc) when ?is_octal(A), A =< $3, ?is_octal(B), ?is_octal(C) ->
  io:format(standard_error, "warning: octals inside strings/sigils/chars are deprecated, got: \\~ts~n", [<<A, B, C>>]),
  append_escaped(Rest, Map, [A,B,C], true, Hex, Acc, 8);

unescape_chars(<<$\\,A,B,Rest/binary>>, Map, true, Hex, Acc) when ?is_octal(A), ?is_octal(B) ->
  io:format(standard_error, "warning: octals inside strings/sigils/chars are deprecated, got: \\~ts~n", [<<A, B>>]),
  append_escaped(Rest, Map, [A,B], true, Hex, Acc, 8);

unescape_chars(<<$\\,$0,Rest/binary>>, Map, true, Hex, Acc) ->
  append_escaped(Rest, Map, [$0], true, Hex, Acc, 8);

unescape_chars(<<$\\,A,Rest/binary>>, Map, true, Hex, Acc) when ?is_octal(A) ->
  io:format(standard_error, "warning: octals inside strings/sigils/chars are deprecated, got: \\~ts~n", [<<A>>]),
  append_escaped(Rest, Map, [A], true, Hex, Acc, 8);

unescape_chars(<<$\\,$X,A,B,Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A,B], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,A,Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,${,A,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,${,A,B,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A,B], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,${,A,B,C,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A,B,C], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,${,A,B,C,D,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A,B,C,D], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,${,A,B,C,D,E,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A,B,C,D,E], Octal, true, Acc, 16);

unescape_chars(<<$\\,$X,${,A,B,C,D,E,F,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  io:format(standard_error, "warning: \\X inside strings/sigils/chars is deprecated, please use \\x instead~n", []),
  append_escaped(Rest, Map, [A,B,C,D,E,F], Octal, true, Acc, 16);

%% End of deprecated

unescape_chars(<<$\\,$x,A,B,Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B) ->
  append_escaped(Rest, Map, [A,B], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,A,Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A) ->
  append_escaped(Rest, Map, [A], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,${,A,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A) ->
  append_escaped(Rest, Map, [A], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,${,A,B,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B) ->
  append_escaped(Rest, Map, [A,B], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,${,A,B,C,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  append_escaped(Rest, Map, [A,B,C], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,${,A,B,C,D,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  append_escaped(Rest, Map, [A,B,C,D], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,${,A,B,C,D,E,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  append_escaped(Rest, Map, [A,B,C,D,E], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,${,A,B,C,D,E,F,$},Rest/binary>>, Map, Octal, true, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  append_escaped(Rest, Map, [A,B,C,D,E,F], Octal, true, Acc, 16);

unescape_chars(<<$\\,$x,_/binary>>, _Map, _Octal, true, _Acc) ->
  Msg = <<"missing hex sequence after \\x">>,
  error('Elixir.ArgumentError':exception([{message,Msg}]));

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
  try <<Acc/binary, Codepoint/utf8>> of
    Binary -> unescape_chars(Rest, Map, Octal, Hex, Binary)
  catch
    error:badarg ->
      Msg = <<"invalid or reserved unicode codepoint ", (integer_to_binary(Codepoint))/binary>>,
      error('Elixir.ArgumentError':exception([{message,Msg}]))
  end.

% Unescape Helpers

unescape_map($0) -> 0;
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
unescape_map($x) -> true;
unescape_map(E)  -> E.

% Extract Helpers

finish_extraction(Line, Buffer, Output, Remaining) ->
  case build_string(Line, Buffer, Output) of
    []    -> Final = [<<>>];
    Final -> []
  end,
  {Line, lists:reverse(Final), Remaining}.

build_string(_Line, [], Output) -> Output;
build_string(_Line, Buffer, Output) ->
  [elixir_utils:characters_to_binary(lists:reverse(Buffer))|Output].

build_interpol(Line, Buffer, Output) ->
  [{Line, lists:reverse(Buffer)}|Output].
