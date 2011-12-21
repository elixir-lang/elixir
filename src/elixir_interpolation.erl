% Handle string and string-like interpolations.
-module(elixir_interpolation).
-export([transform/3, extract/4, unescape_chars/2]).
-include("elixir.hrl").

% Extract string interpolations

extract(Line, Escaping, String, Last) ->
  extract(Line, Escaping, String, [], [], [], Last).

extract(Line, Escaping, [], Buffer, [], Output, []) ->
  { lists:reverse(build_interpol(s, Escaping, Buffer, Output)), [] };

extract(Line, Escaping, [Last|Remaining], Buffer, [], Output, Last) ->
  { lists:reverse(build_interpol(s, Escaping, Buffer, Output)), Remaining };

extract(Line, Escaping, [Last], Buffer, Search, Output, Last) ->
  { error, io_lib:format("unexpected end of string, expected ~ts", [[hd(Search)]]) };

extract(Line, Escaping, [$\\, $#, ${|Rest], Buffer, [], Output, Last) ->
  extract(Line, Escaping, Rest, [${,$#|Buffer], [], Output, Last);

extract(Line, Escaping, [$\\,Char|Rest], Buffer, [], Output, Last) ->
  extract(Line, Escaping, Rest, [Char,$\\|Buffer], [], Output, Last);

extract(Line, Escaping, [$#, ${|Rest], Buffer, [], Output, Last) ->
  NewOutput = build_interpol(s, Escaping, Buffer, Output),
  extract(Line, Escaping, Rest, [], [$}], NewOutput, Last);

extract(Line, Escaping, [$}|Rest], Buffer, [$}], Output, Last) ->
  NewOutput = build_interpol(i, Escaping, Buffer, Output),
  extract(Line, Escaping, Rest, [], [], NewOutput, Last);

% Check for available separators "", {}, [] and () inside interpolation

extract(Line, Escaping, [C|Rest], Buffer, [C|Search], Output, Last) when C == $); C == $]; C == $}; C == $"; C == $' ->
  extract(Line, Escaping, Rest, [C|Buffer], Search, Output, Last);

extract(Line, Escaping, [C|Rest], Buffer, Search, Output, Last) when C == $"; C == $' ->
  extract(Line, Escaping, Rest, [C|Buffer], [C|Search], Output, Last);

extract(Line, Escaping, [${|Rest], Buffer, Search, Output, Last) ->
  extract(Line, Escaping, Rest, [${|Buffer], [$}|Search], Output, Last);

extract(Line, Escaping, [$[|Rest], Buffer, Search, Output, Last) ->
  extract(Line, Escaping, Rest, [$[|Buffer], [$]|Search], Output, Last);

extract(Line, Escaping, [$(|Rest], Buffer, Search, Output, Last) ->
  extract(Line, Escaping, Rest, [$(|Buffer], [$)|Search], Output, Last);

% Else

extract(Line, Escaping, [Char|Rest], Buffer, Search, Output, Last) ->
  extract(Line, Escaping, Rest, [Char|Buffer], Search, Output, Last).

% Handle interpolation. The final result will be a parse tree that
% returns a flattened list.

transform(String, Line, S) ->
  Interpolations = String,

  % Optimized cases interpolations actually has no interpolation.
  case Interpolations of
    [{s, String}] ->
      { Final, FS, _ } = handle_string_extractions(hd(Interpolations), Line, S),
      { Final, FS };
    _ ->
      Transformer = fun(X, Acc) -> handle_string_extractions(X, Line, S) end,
      elixir_tree_helpers:build_bin(Transformer, Interpolations, Line, S)
  end.

% Unescape chars. For instance, "\" "n" (two chars) needs to be converted to "\n" (one char).

unescape_chars(Escaping, String) -> unescape_chars(Escaping, String, []).
unescape_chars(Escaping, [], Output) -> lists:reverse(Output);

% Do not escape everything, just a few. Used by regular expressions.
unescape_chars(regexp, [$\\, Escaped|Rest], Output) ->
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

% Escape characters as in strings.
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
  unescape_chars(Escaping, Rest, [Char|Output]).

% Helpers

handle_string_extractions({s, String}, Line, S) ->
  { { string, Line, String }, S, default };

handle_string_extractions({i, Interpolation}, Line, S) ->
  { Tree, NS } = elixir_transform:parse(Interpolation, Line, S),
  % Stringify = elixir_tree_helpers:build_method_call(to_s, Line, [], hd(Tree)),
  { hd(Tree), NS, [binary] }.

build_interpol(Piece, Escaping, [], Output) ->
  Output;

build_interpol(s, Escaping, Buffer, Output) ->
  [{s, unescape_chars(Escaping, lists:reverse(Buffer))}|Output];

build_interpol(i, Escaping, Buffer, Output) ->
  [{i, lists:reverse(Buffer)}|Output].

extract_integers([H|T], Acc) when H >= 48 andalso H =< 57 -> 
  extract_integers(T, [H|Acc]);

extract_integers(Remaining, Acc) ->
  { Remaining, lists:reverse(Acc) }.
