% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/6, unescape_chars/1, unescape_chars/2,
unescape_tokens/1, unescape_tokens/2, unescape_map/1]).
-include("elixir.hrl").
-define(is_hex(S), ((S >= $0 andalso S =< $9) orelse
                    (S >= $A andalso S =< $F) orelse
                    (S >= $a andalso S =< $f))).

%% Extract string interpolations

extract(Line, Column, Raw, Interpol, String, Last) ->
  %% Ignore whatever is in the scope and enable terminator checking.
  Scope = Raw#elixir_tokenizer{terminators=[], check_terminators=true},
  extract(Line, Column, Scope, Interpol, String, [], [], Last).

%% Terminators

extract(Line, Column, _Scope, _Interpol, [], Buffer, Output, none) ->
  finish_extraction(Line, Column, Buffer, Output, []);

extract(Line, Column, _Scope, _Interpol, [], _Buffer, _Output, Last) ->
  {error, {string, Line, Column, io_lib:format("missing terminator: ~ts", [[Last]]), []}};

extract(Line, Column, _Scope, _Interpol, [Last | Remaining], Buffer, Output, Last) ->
  finish_extraction(Line, Column + 1, Buffer, Output, Remaining);

%% Going through the string

extract(Line, _Column, Scope, true, [$\\, $\n | Rest], Buffer, Output, Last) ->
  NewBuffer =
    case Scope#elixir_tokenizer.unescape of
      true -> Buffer;
      false -> [$\n, $\\ |  Buffer]
    end,
  extract(Line+1, 1, Scope, true, Rest, NewBuffer, Output, Last);

extract(Line, _Column, Scope, true, [$\\, $\r, $\n | Rest], Buffer, Output, Last) ->
  NewBuffer =
    case Scope#elixir_tokenizer.unescape of
      true -> Buffer;
      false -> [$\n, $\r, $\\ |  Buffer]
    end,
  extract(Line+1, 1, Scope, true, Rest, NewBuffer, Output, Last);

extract(Line, _Column, Scope, Interpol, [$\n | Rest], Buffer, Output, Last) ->
  extract(Line+1, 1, Scope, Interpol, Rest, [$\n | Buffer], Output, Last);

extract(Line, Column, Scope, Interpol, [$\\, Last | Rest], Buffer, Output, Last) ->
  extract(Line, Column+2, Scope, Interpol, Rest, [Last | Buffer], Output, Last);

extract(Line, Column, Scope, true, [$\\, $#, ${ | Rest], Buffer, Output, Last) ->
  extract(Line, Column+1, Scope, true, Rest, [${, $#, $\\ | Buffer], Output, Last);

extract(Line, Column, Scope, true, [$#, ${ | Rest], Buffer, Output, Last) ->
  Output1 = build_string(Line, Buffer, Output),
  case elixir_tokenizer:tokenize(Rest, Line, Column + 2, Scope) of
    {error, {EndLine, EndColumn, _, "}"}, [$} | NewRest], Tokens} ->
      Output2 = build_interpol(Line, Column, EndLine, EndColumn, Tokens, Output1),
      extract(EndLine, EndColumn + 1, Scope, true, NewRest, [], Output2, Last);
    {error, Reason, _, _} ->
      {error, Reason};
    {ok, _} ->
      {error, {string, Line, Column, "missing interpolation terminator: \"}\"", []}}
  end;

extract(Line, Column, Scope, Interpol, [$\\, Char | Rest], Buffer, Output, Last) ->
  extract(Line, Column+2, Scope, Interpol, Rest, [Char, $\\ | Buffer], Output, Last);

%% Catch all clause

extract(Line, Column, Scope, Interpol, [Char | Rest], Buffer, Output, Last) ->
  extract(Line, Column + 1, Scope, Interpol, Rest, [Char | Buffer], Output, Last).

%% Unescape a series of tokens as returned by extract.

unescape_tokens(Tokens) ->
  unescape_tokens(Tokens, fun unescape_map/1).

unescape_tokens(Tokens, Map) ->
  try [unescape_token(Token, Map) || Token <- Tokens] of
    Unescaped -> {ok, Unescaped}
  catch
    {error, _Reason} = Error -> Error
  end.

unescape_token(Token, Map) when is_list(Token) ->
  unescape_chars(elixir_utils:characters_to_binary(Token), Map);
unescape_token(Token, Map) when is_binary(Token) ->
  unescape_chars(Token, Map);
unescape_token(Other, _Map) ->
  Other.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) ->
  unescape_chars(String, fun unescape_map/1).

unescape_chars(String, Map) ->
  unescape_chars(String, Map, <<>>).

unescape_chars(<<$\\, $x, Rest/binary>>, Map, Acc) ->
  case Map(hex) of
    true  -> unescape_hex(Rest, Map, Acc);
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, $x>>)
  end;

unescape_chars(<<$\\, $u, Rest/binary>>, Map, Acc) ->
  case Map(unicode) of
    true  -> unescape_unicode(Rest, Map, Acc);
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, $u>>)
  end;

unescape_chars(<<$\\, Escaped, Rest/binary>>, Map, Acc) ->
  case Map(Escaped) of
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, Escaped>>);
    Other -> unescape_chars(Rest, Map, <<Acc/binary, Other>>)
  end;

unescape_chars(<<Char, Rest/binary>>, Map, Acc) ->
  unescape_chars(Rest, Map, <<Acc/binary, Char>>);

unescape_chars(<<>>, _Map, Acc) -> Acc.

% Unescape Helpers

unescape_hex(<<A, B, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B) ->
  Bytes = list_to_integer([A, B], 16),
  unescape_chars(Rest, Map, <<Acc/binary, Bytes>>);

%% TODO: Remove deprecated sequences on v2.0

unescape_hex(<<A, Rest/binary>>, Map, Acc) when ?is_hex(A) ->
  io:format(standard_error, "warning: \\xH inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A], Acc, 16);

unescape_hex(<<${, A, $}, Rest/binary>>, Map, Acc) when ?is_hex(A) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A], Acc, 16);

unescape_hex(<<${, A, B, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A, B], Acc, 16);

unescape_hex(<<${, A, B, C, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A, B, C], Acc, 16);

unescape_hex(<<${, A, B, C, D, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A, B, C, D], Acc, 16);

unescape_hex(<<${, A, B, C, D, E, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A, B, C, D, E], Acc, 16);

unescape_hex(<<${, A, B, C, D, E, F, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  io:format(standard_error, "warning: \\x{H*} inside strings/sigils/chars is deprecated, please use \\xHH (byte) or \\uHHHH (code point) instead~n", []),
  append_codepoint(Rest, Map, [A, B, C, D, E, F], Acc, 16);

unescape_hex(<<_/binary>>, _Map, _Acc) ->
  throw({error, "missing hex sequence after \\x, expected \\xHH"}).

%% Finish deprecated sequences

unescape_unicode(<<A, B, C, D, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  append_codepoint(Rest, Map, [A, B, C, D], Acc, 16);

unescape_unicode(<<${, A, $}, Rest/binary>>, Map, Acc) when ?is_hex(A) ->
  append_codepoint(Rest, Map, [A], Acc, 16);

unescape_unicode(<<${, A, B, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B) ->
  append_codepoint(Rest, Map, [A, B], Acc, 16);

unescape_unicode(<<${, A, B, C, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C) ->
  append_codepoint(Rest, Map, [A, B, C], Acc, 16);

unescape_unicode(<<${, A, B, C, D, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D) ->
  append_codepoint(Rest, Map, [A, B, C, D], Acc, 16);

unescape_unicode(<<${, A, B, C, D, E, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E) ->
  append_codepoint(Rest, Map, [A, B, C, D, E], Acc, 16);

unescape_unicode(<<${, A, B, C, D, E, F, $}, Rest/binary>>, Map, Acc) when ?is_hex(A), ?is_hex(B), ?is_hex(C), ?is_hex(D), ?is_hex(E), ?is_hex(F) ->
  append_codepoint(Rest, Map, [A, B, C, D, E, F], Acc, 16);

unescape_unicode(<<_/binary>>, _Map, _Acc) ->
  Msg = <<"invalid Unicode sequence after \\u, expected \\uHHHH or \\u{H*}">>,
  error('Elixir.ArgumentError':exception([{message, Msg}])).

append_codepoint(Rest, Map, List, Acc, Base) ->
  Codepoint = list_to_integer(List, Base),
  try <<Acc/binary, Codepoint/utf8>> of
    Binary -> unescape_chars(Rest, Map, Binary)
  catch
    error:badarg ->
      Msg = <<"invalid or reserved Unicode code point ", (integer_to_binary(Codepoint))/binary>>,
      error('Elixir.ArgumentError':exception([{message, Msg}]))
  end.

unescape_map(unicode) -> true;
unescape_map(hex) -> true;
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
unescape_map(E)  -> E.

% Extract Helpers

finish_extraction(Line, Column, Buffer, Output, Remaining) ->
  Final = case build_string(Line, Buffer, Output) of
    [] -> [[]];
    F  -> F
  end,

  {Line, Column, lists:reverse(Final), Remaining}.

build_string(_Line, [], Output) -> Output;
build_string(_Line, Buffer, Output) -> [lists:reverse(Buffer) | Output].

build_interpol(Line, Column, EndLine, EndColumn, Buffer, Output) ->
  [{{Line, Column, nil}, {EndLine, EndColumn, nil}, lists:reverse(Buffer)} | Output].
