% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/6, unescape_string/1, unescape_string/2,
unescape_tokens/1, unescape_map/1]).
-include("elixir.hrl").
-include("elixir_tokenizer.hrl").

%% Extract string interpolations

extract(Line, Column, Scope, Interpol, String, Last) ->
  extract(String, [], [], Line, Column, Scope, Interpol, Last).

%% Terminators

extract([], _Buffer, _Output, Line, Column, #elixir_tokenizer{cursor_completion=false}, _Interpol, Last) ->
  {error, {string, Line, Column, io_lib:format("missing terminator: ~ts", [[Last]]), []}};

extract([], Buffer, Output, Line, Column, Scope, _Interpol, _Last) ->
  finish_extraction([], Buffer, Output, Line, Column, Scope);

extract([Last | Rest], Buffer, Output, Line, Column, Scope, _Interpol, Last) ->
  finish_extraction(Rest, Buffer, Output, Line, Column + 1, Scope);

%% Going through the string

extract([$\\, $\r, $\n | Rest], Buffer, Output, Line, _Column, Scope, Interpol, Last) ->
  extract_nl(Rest, [$\n, $\r, $\\ | Buffer], Output, Line, Scope, Interpol, Last);

extract([$\\, $\n | Rest], Buffer, Output, Line, _Column, Scope, Interpol, Last) ->
  extract_nl(Rest, [$\n, $\\ | Buffer], Output, Line, Scope, Interpol, Last);

extract([$\n | Rest], Buffer, Output, Line, _Column, Scope, Interpol, Last) ->
  extract_nl(Rest, [$\n | Buffer], Output, Line, Scope, Interpol, Last);

extract([$\\, Last | Rest], Buffer, Output, Line, Column, Scope, Interpol, Last) ->
  NewScope =
    %% TODO: Remove this on Elixir v2.0
    case Interpol of
      true ->
        Scope;
      false ->
        Msg = "using \\~ts to escape the closing of an uppercase sigil is deprecated, please use another delimiter or a lowercase sigil instead",
        prepend_warning(Line, Column, io_lib:format(Msg, [[Last]]), Scope)
    end,

  extract(Rest, [Last | Buffer], Output, Line, Column+2, NewScope, Interpol, Last);

extract([$\\, Last, Last, Last | Rest], Buffer, Output, Line, Column, Scope, Interpol, [Last, Last, Last] = All) ->
  extract(Rest, [Last, Last, Last | Buffer], Output, Line, Column+4, Scope, Interpol, All);

extract([$\\, $#, ${ | Rest], Buffer, Output, Line, Column, Scope, true, Last) ->
  extract(Rest, [${, $#, $\\ | Buffer], Output, Line, Column+3, Scope, true, Last);

extract([$#, ${ | Rest], Buffer, Output, Line, Column, Scope, true, Last) ->
  Output1 = build_string(Buffer, Output),
  case elixir_tokenizer:tokenize(Rest, Line, Column + 2, Scope#elixir_tokenizer{terminators=[]}) of
    {error, {Location, _, "}"}, [$} | NewRest], Warnings, Tokens} ->
      NewScope = Scope#elixir_tokenizer{warnings=Warnings},
      {line, EndLine} = lists:keyfind(line, 1, Location),
      {column, EndColumn} = lists:keyfind(column, 1, Location),
      Output2 = build_interpol(Line, Column, EndLine, EndColumn, lists:reverse(Tokens), Output1),
      extract(NewRest, [], Output2, EndLine, EndColumn + 1, NewScope, true, Last);
    {error, Reason, _, _, _} ->
      {error, Reason};
    {ok, EndLine, EndColumn, Warnings, Tokens, Terminators} when Scope#elixir_tokenizer.cursor_completion /= false ->
      NewScope = Scope#elixir_tokenizer{warnings=Warnings, cursor_completion=noprune},
      Output2 = build_interpol(Line, Column, EndLine, EndColumn, lists:reverse(Tokens, Terminators), Output1),
      extract([], [], Output2, EndLine, EndColumn, NewScope, true, Last);
    {ok, _, _, _, _, _} ->
      {error, {string, Line, Column, "missing interpolation terminator: \"}\"", []}}
  end;

extract([$\\ | Rest], Buffer, Output, Line, Column, Scope, Interpol, Last) ->
  extract_char(Rest, [$\\ | Buffer], Output, Line, Column + 1, Scope, Interpol, Last);

%% Catch all clause

extract([Char1, Char2 | Rest], Buffer, Output, Line, Column, Scope, Interpol, Last)
    when Char1 =< 255, Char2 =< 255 ->
  extract([Char2 | Rest], [Char1 | Buffer], Output, Line, Column + 1, Scope, Interpol, Last);

extract(Rest, Buffer, Output, Line, Column, Scope, Interpol, Last) ->
  extract_char(Rest, Buffer, Output, Line, Column, Scope, Interpol, Last).

extract_char(Rest, Buffer, Output, Line, Column, Scope, Interpol, Last) ->
  case unicode_util:gc(Rest) of
    [Char | _] when ?bidi(Char) ->
      Token = io_lib:format("\\u~4.16.0B", [Char]),
      Pre = "invalid bidirectional formatting character in string: ",
      Pos = io_lib:format(". If you want to use such character, use it in its escaped ~ts form instead", [Token]),
      {error, {?LOC(Line, Column), {Pre, Pos}, Token}};

    [Char | NewRest] when is_list(Char) ->
      extract(NewRest, lists:reverse(Char, Buffer), Output, Line, Column + 1, Scope, Interpol, Last);

    [Char | NewRest] when is_integer(Char) ->
      extract(NewRest, [Char | Buffer], Output, Line, Column + 1, Scope, Interpol, Last);

    [] ->
      extract([], Buffer, Output, Line, Column, Scope, Interpol, Last)
  end.

%% Handle newlines. Heredocs require special attention

extract_nl(Rest, Buffer, Output, Line, Scope, Interpol, [H,H,H] = Last) ->
  case strip_horizontal_space(Rest, Buffer, 1) of
    {[H,H,H|NewRest], _NewBuffer, Column} ->
      finish_extraction(NewRest, Buffer, Output, Line + 1, Column + 3, Scope);
    {NewRest, NewBuffer, Column} ->
      extract(NewRest, NewBuffer, Output, Line + 1, Column, Scope, Interpol, Last)
  end;
extract_nl(Rest, Buffer, Output, Line, Scope, Interpol, Last) ->
  extract(Rest, Buffer, Output, Line + 1, Scope#elixir_tokenizer.column, Scope, Interpol, Last).

strip_horizontal_space([H | T], Buffer, Counter) when H =:= $\s; H =:= $\t ->
  strip_horizontal_space(T, [H | Buffer], Counter + 1);
strip_horizontal_space(T, Buffer, Counter) ->
  {T, Buffer, Counter}.

%% Unescape a series of tokens as returned by extract.

unescape_tokens(Tokens) ->
  try [unescape_token(Token, fun unescape_map/1) || Token <- Tokens] of
    Unescaped -> {ok, Unescaped}
  catch
    {error, _Reason, _Token} = Error -> Error
  end.

unescape_token(Token, Map) when is_list(Token) ->
  unescape_chars(elixir_utils:characters_to_binary(Token), Map);
unescape_token(Token, Map) when is_binary(Token) ->
  unescape_chars(Token, Map);
unescape_token(Other, _Map) ->
  Other.

% Unescape string. This is called by Elixir. Wrapped by convenience.

unescape_string(String) ->
  unescape_string(String, fun unescape_map/1).

unescape_string(String, Map) ->
  try
    unescape_chars(String, Map)
  catch
    {error, Reason, _} ->
      Message = elixir_utils:characters_to_binary(Reason),
      error('Elixir.ArgumentError':exception([{message, Message}]))
  end.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

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

unescape_chars(<<$\\, $\n, Rest/binary>>, Map, Acc) ->
  case Map(newline) of
    true  -> unescape_chars(Rest, Map, Acc);
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, $\n>>)
  end;

unescape_chars(<<$\\, $\r, $\n, Rest/binary>>, Map, Acc) ->
  case Map(newline) of
    true  -> unescape_chars(Rest, Map, Acc);
    false -> unescape_chars(Rest, Map, <<Acc/binary, $\\, $\r, $\n>>)
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
  throw({error, "invalid hex escape character, expected \\xHH where H is a hexadecimal digit", "\\x"}).

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
  throw({error, "invalid Unicode escape character, expected \\uHHHH or \\u{H*} where H is a hexadecimal digit", "\\u"}).

append_codepoint(Rest, Map, List, Acc, Base) ->
  Codepoint = list_to_integer(List, Base),
  try <<Acc/binary, Codepoint/utf8>> of
    Binary -> unescape_chars(Rest, Map, Binary)
  catch
    error:badarg ->
      throw({error, "invalid or reserved Unicode code point \\u{" ++ List ++ "}", "\\u"})
  end.

unescape_map(newline) -> true;
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

finish_extraction(Remaining, Buffer, Output, Line, Column, Scope) ->
  Final = case build_string(Buffer, Output) of
    [] -> [[]];
    F  -> F
  end,

  {Line, Column, lists:reverse(Final), Remaining, Scope}.

build_string([], Output) -> Output;
build_string(Buffer, Output) -> [lists:reverse(Buffer) | Output].

build_interpol(Line, Column, EndLine, EndColumn, Buffer, Output) ->
  [{{Line, Column, nil}, {EndLine, EndColumn, nil}, Buffer} | Output].

prepend_warning(Line, Column, Msg, #elixir_tokenizer{warnings=Warnings} = Scope) ->
  Scope#elixir_tokenizer{warnings = [{{Line, Column}, Msg} | Warnings]}.
