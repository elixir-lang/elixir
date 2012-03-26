% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/4, unescape_chars/1, unescape_tokens/1]).
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
  [unescape_token(Token) || Token <- Tokens].

unescape_token(Token) when is_list(Token) -> unescape_chars(Token);
unescape_token(Other) -> Other.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(String) -> unescape_chars(String, []).

unescape_chars([$\\, Escaped|Rest], Output) ->
  case extract_integers([Escaped|Rest], []) of
    {_,[]} ->
      Char = case Escaped of
        $b  -> $\b;
        $d  -> $\d;
        $e  -> $\e;
        $f  -> $\f;
        $n  -> $\n;
        $r  -> $\r;
        $s  -> $\s;
        $t  -> $\t;
        $v  -> $\v;
        _   -> Escaped
      end,
      unescape_chars(Rest, [Char|Output]);
    {RealRest,Integer} ->
      unescape_chars(RealRest, [list_to_integer(Integer)|Output])
  end;

unescape_chars([Char|Rest], Output) ->
  unescape_chars(Rest, [Char|Output]);

unescape_chars([], Output) -> lists:reverse(Output).

% Unescape Helpers

extract_integers([H|T], Acc) when H >= $0 andalso H =< $9 ->
  extract_integers(T, [H|Acc]);

extract_integers(Remaining, Acc) ->
  { Remaining, lists:reverse(Acc) }.

% Extract Helpers

finish_extraction(Line, Buffer, Output, Remaining) ->
  case build_interpol(s, Line, Buffer, Output) of
    []    -> Final = [[]];
    Final -> []
  end,
  { Line, lists:reverse(Final), Remaining }.

build_interpol(_Kind, _Line, [], Output) ->
  Output;

build_interpol(s, _Line, Buffer, Output) ->
  [lists:reverse(Buffer)|Output];

build_interpol(i, Line, Buffer, Output) ->
  [wrap_interpol(Line, forms(lists:reverse(Buffer), Line))| Output].

wrap_interpol(_Line, Form) when is_binary(Form) ->
  Form;

wrap_interpol(Line, Form) ->
  { '|', Line, [{ { '.', Line, ['__MAIN__.String.Chars', to_binary] }, Line, [Form]}, binary]}.

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