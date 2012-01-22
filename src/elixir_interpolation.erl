% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([extract/4, unescape_chars/2]).
-include("elixir.hrl").

% Extract string interpolations

extract(Line, Escaping, String, Last) ->
  extract(Line, Escaping, String, [], [], [], Last).

extract(Line, Escaping, [], Buffer, [], Output, []) ->
  finish_extraction(Line, Escaping, Buffer, Output, []);

extract(Line, Escaping, [Last|Remaining], Buffer, [], Output, Last) ->
  finish_extraction(Line, Escaping, Buffer, Output, Remaining);

extract(Line, _Escaping, [Last], _Buffer, Search, _Output, Last) ->
  { error, { Line, io_lib:format("unexpected end of string, expected ~ts", [[hd(Search)]]), [Last] } };

extract(Line, Escaping, [$\n|Rest], Buffer, Search, Output, Last) ->
  extract(Line+1, Escaping, Rest, [$\n|Buffer], Search, Output, Last);

extract(Line, Escaping, [$\\, $#, ${|Rest], Buffer, [], Output, Last) ->
  extract(Line, Escaping, Rest, [${,$#|Buffer], [], Output, Last);

extract(Line, Escaping, [$\\,Char|Rest], Buffer, [], Output, Last) ->
  extract(Line, Escaping, Rest, [Char,$\\|Buffer], [], Output, Last);

extract(Line, Escaping, [$#, ${|Rest], Buffer, [], Output, Last) ->
  NewOutput = build_interpol(s, Line, Escaping, Buffer, Output),
  extract(Line, Escaping, Rest, [], [$}], NewOutput, Last);

extract(Line, Escaping, [$}|Rest], Buffer, [$}], Output, Last) ->
  NewOutput = build_interpol(i, Line, Escaping, Buffer, Output),
  extract(Line, Escaping, Rest, [], [], NewOutput, Last);

% Check for available separators "", {}, [] and () inside interpolation

extract(Line, Escaping, [C|Rest], Buffer, [C|Search], Output, Last) when C == $); C == $]; C == $}; C == $"; C == $' ->
  extract(Line, Escaping, Rest, [C|Buffer], Search, Output, Last);

extract(Line, Escaping, [C|Rest], Buffer, [_|_] = Search, Output, Last) when C == $"; C == $' ->
  extract(Line, Escaping, Rest, [C|Buffer], [C|Search], Output, Last);

extract(Line, Escaping, [${|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, Escaping, Rest, [${|Buffer], [$}|Search], Output, Last);

extract(Line, Escaping, [$[|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, Escaping, Rest, [$[|Buffer], [$]|Search], Output, Last);

extract(Line, Escaping, [$(|Rest], Buffer, [_|_] = Search, Output, Last) ->
  extract(Line, Escaping, Rest, [$(|Buffer], [$)|Search], Output, Last);

% Else

extract(Line, Escaping, [Char|Rest], Buffer, Search, Output, Last) ->
  extract(Line, Escaping, Rest, [Char|Buffer], Search, Output, Last).

finish_extraction(Line, Escaping, Buffer, Output, Remaining) ->
  case build_interpol(s, Line, Escaping, Buffer, Output) of
    []    -> Final = [[]];
    Final -> []
  end,
  { Line, lists:reverse(Final), Remaining }.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(Escaping, String) -> unescape_chars(Escaping, String, []).

unescape_chars(false, [$\\, Escaped|Rest], Output) ->
  case extract_integers([Escaped|Rest], []) of
    {_,[]} ->
      Char = case Escaped of
        $f  -> $\f;
        $n  -> $\n;
        $r  -> $\r;
        $t  -> $\t;
        $v  -> $\v;
        _   -> []
      end,

      case Char of
        [] -> unescape_chars(false, Rest, [Escaped, $\\|Output]);
        _  -> unescape_chars(false, Rest, [Char|Output])
      end;
    {RealRest,Integer} ->
      unescape_chars(true, RealRest, [list_to_integer(Integer)|Output])
  end;

unescape_chars(_, [$\\, Escaped|Rest], Output) ->
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
      unescape_chars(true, Rest, [Char|Output]);
    {RealRest,Integer} ->
      unescape_chars(true, RealRest, [list_to_integer(Integer)|Output])
  end;

unescape_chars(Escaping, [Char|Rest], Output) ->
  unescape_chars(Escaping, Rest, [Char|Output]);

unescape_chars(_Escaping, [], Output) -> lists:reverse(Output).

% Helpers

build_interpol(_Kind, _Line, _Escaping, [], Output) ->
  Output;

build_interpol(s, _Line, Escaping, Buffer, Output) ->
  [unescape_chars(Escaping, lists:reverse(Buffer))|Output];

build_interpol(i, Line, _Escaping, Buffer, Output) ->
  [wrap_interpol(Line, forms(lists:reverse(Buffer), Line))| Output].

wrap_interpol(_Line, Form) when is_binary(Form) ->
  Form;

wrap_interpol(Line, Form) ->
  { '|', Line, [{ { '.', Line, ['::String::Inspect', to_binary] }, Line, [Form]}, binary]}.

extract_integers([H|T], Acc) when H >= 48 andalso H =< 57 ->
  extract_integers(T, [H|Acc]);

extract_integers(Remaining, Acc) ->
  { Remaining, lists:reverse(Acc) }.

forms(String, StartLine) ->
  case elixir_tokenizer:tokenize(String, StartLine) of
    {ok, Tokens} ->
      case elixir_parser:parse(Tokens) of
        {ok, [Forms]} when not is_list(Forms) -> Forms;
        {ok, Forms} -> { block, StartLine, Forms };
        {error, {Line, _, [Error, Token]}} -> throw({ interpolation_error, { Line, Error, Token } })
      end;
    {error, {Line, Error, Token}} -> throw({ interpolation_error, { Line, Error, Token } })
  end.