% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/4, unescape_chars/1, unescape_chars/2,
unescape_tokens/1, unescape_tokens/2, unescape_map/1]).
-define(is_octal(S), S >= $0 andalso S =< $7).
-include("elixir.hrl").

%% Extract string interpolations

extract(Line, Interpol, String, Last) ->
  extract(Line, Interpol, String, [], [], [], Last).

extract(Line, _Interpol, [], Buffer, [], Output, []) ->
  finish_extraction(Line, Buffer, Output, []);

extract(Line, _Interpol, [], _Buffer, [], _Output, Last) ->
  { error, { Line, io_lib:format("missing string terminator, expected ~ts", [[Last]]), [] } };

extract(Line, _Interpol, [Last|Remaining], Buffer, [], Output, Last) ->
  finish_extraction(Line, Buffer, Output, Remaining);

extract(Line, _Interpol, [Last], _Buffer, Search, _Output, Last) ->
  { error, { Line, io_lib:format("unexpected end of string, expected ~ts", [[hd(Search)]]), [Last] } };

extract(Line, Interpol, [$\n|Rest], Buffer, Search, Output, Last) ->
  extract(Line+1, Interpol, Rest, [$\n|Buffer], Search, Output, Last);

extract(Line, Interpol, [$\\, $#, ${|Rest], Buffer, [], Output, Last) ->
  extract(Line, Interpol, Rest, [${,$#|Buffer], [], Output, Last);

extract(Line, Interpol, [$\\,Char|Rest], Buffer, [], Output, Last) ->
  extract(Line, Interpol, Rest, [Char,$\\|Buffer], [], Output, Last);

extract(Line, true, [$#, ${|Rest], Buffer, [], Output, Last) ->
  NewOutput = build_interpol(s, Line, Buffer, Output),
  extract(Line, true, Rest, [], [$}], NewOutput, Last);

extract(Line, true, [$}|Rest], Buffer, [$}], Output, Last) ->
  NewOutput = build_interpol(i, Line, Buffer, Output),
  extract(Line, true, Rest, [], [], NewOutput, Last);

%% Check for available separators "", {}, [] and () inside interpolation

extract(Line, Interpol, [C|Rest], Buffer, [C|Search], Output, Last) when C == $); C == $]; C == $}; C == $"; C == $' ->
  extract(Line, Interpol, Rest, [C|Buffer], Search, Output, Last);

extract(Line, Interpol, [C|Rest], Buffer, [_|_] = Search, Output, Last) when C == $"; C == $' ->
  extract(Line, Interpol, Rest, [C|Buffer], [C|Search], Output, Last);

extract(Line, Interpol, [${|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, Interpol, Rest, [${|Buffer], [$}|Search], Output, Last);

extract(Line, Interpol, [$[|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, Interpol, Rest, [$[|Buffer], [$]|Search], Output, Last);

extract(Line, Interpol, [$(|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, Interpol, Rest, [$(|Buffer], [$)|Search], Output, Last);

%% Else

extract(Line, Interpol, [Char|Rest], Buffer, Search, Output, Last) ->
  extract(Line, Interpol, Rest, [Char|Buffer], Search, Output, Last).

%% Unescape a series of tokens as returned by extract.

unescape_tokens(Tokens) ->
  unescape_tokens(Tokens, fun unescape_map/1).

unescape_tokens(Tokens, Map) ->
  [unescape_token(Token, Map) || Token <- Tokens].

unescape_token(Token, Map) when is_binary(Token) -> unescape_chars(Token, Map);
unescape_token(Other, _Map) -> Other.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) -> unescape_chars(String, fun unescape_map/1).

-define(to_octal(List),
  <<(list_to_integer(List, 8))/integer, (unescape_chars(Rest, Map))/binary>>
).

unescape_chars(<<$\\,A,B,C,Rest/binary>>, Map) when ?is_octal(A), ?is_octal(B), ?is_octal(C) ->
  ?to_octal([A,B,C]);

unescape_chars(<<$\\,A,B,Rest/binary>>, Map) when ?is_octal(A), ?is_octal(B) ->
  ?to_octal([A,B]);

unescape_chars(<<$\\,A,Rest/binary>>, Map) when ?is_octal(A) ->
  ?to_octal([A]);

unescape_chars(<<$\\,Escaped,Rest/binary>>, Map) ->
  case Map(Escaped) of
    false -> <<$\\,Escaped,(unescape_chars(Rest, Map))/binary>>;
    Other -> <<Other,(unescape_chars(Rest, Map))/binary>>
  end;

unescape_chars(<<Char, Rest/binary>>, Map) ->
  <<Char, (unescape_chars(Rest, Map))/binary>>;

unescape_chars(<<>>, _Map) -> <<>>.

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

finish_extraction(Line, Buffer, Output, Remaining) ->
  case build_interpol(s, Line, Buffer, Output) of
    []    -> Final = [<<>>];
    Final -> []
  end,
  { Line, lists:reverse(Final), Remaining }.

build_interpol(_Kind, _Line, [], Output) ->
  Output;

build_interpol(s, _Line, Buffer, Output) ->
  [unicode:characters_to_binary(lists:reverse(Buffer))|Output];

build_interpol(i, Line, Buffer, Output) ->
  [wrap_interpol(Line, forms(lists:reverse(Buffer), Line))| Output].

wrap_interpol(_Line, Form) when is_binary(Form) ->
  Form;

wrap_interpol(Line, Form) ->
  { '|', Line, [{ { '.', Line, ['__MAIN__.Binary.Chars', to_binary] }, Line, [Form]}, binary]}.

forms(String, StartLine) ->
  case elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      case elixir_parser:parse(Tokens) of
        {ok, [Forms]} when not is_list(Forms) -> Forms;
        {ok, Forms} -> { '__block__', StartLine, Forms };
        {error, {Line, _, [Error, Token]}} -> throw({ interpolation_error, { Line, Error, Token } })
      end;
    {error, {Line, Error, Token}} -> throw({ interpolation_error, { Line, Error, Token } })
  end.