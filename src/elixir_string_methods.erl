-module(elixir_string_methods).
-export([add/2, extract_interpolations/1]).
-include("elixir.hrl").

add(Left, Right) ->
  <<Left/binary, Right/binary>>.

extract_interpolations(String) ->
  extract_interpolations(String, [], [], []).

extract_interpolations([], Buffer, [], Output) ->
  lists:reverse(build_interpol(s, Buffer, Output));

extract_interpolations([], Buffer, Search, Output) ->
  elixir_errors:raise(badarg, "unexpected end of string, expected ~ts", [[hd(Search)]]);

extract_interpolations([$\\, $#, ${|Rest], Buffer, [], Output) ->
  extract_interpolations(Rest, [${,$#|Buffer], [], Output);

extract_interpolations([$#, ${|Rest], Buffer, [], Output) ->
  NewOutput = build_interpol(s, Buffer, Output),
  extract_interpolations(Rest, [], [$}], NewOutput);

extract_interpolations([Char|Rest], Buffer, [], Output) ->
  extract_interpolations(Rest, [Char|Buffer], [], Output);

extract_interpolations([$}|Rest], Buffer, [$}], Output) ->
  NewOutput = build_interpol(i, Buffer, Output),
  extract_interpolations(Rest, [], [], NewOutput);

extract_interpolations([$\\,Char|Rest], Buffer, [], Output) ->
  extract_interpolations(Rest, [Char,$\\|Buffer], [], Output);

% Check for available separators: "", {}, [] and ()

extract_interpolations([$"|Rest], Buffer, [$"|Search], Output) ->
  extract_interpolations(Rest, [$"|Buffer], Search, Output);

extract_interpolations([$"|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [$"|Buffer], [$"|Search], Output);

extract_interpolations([${|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [${|Buffer], [$}|Search], Output);

extract_interpolations([$}|Rest], Buffer, [$}|Search], Output) ->
  extract_interpolations(Rest, [$}|Buffer], Search, Output);

extract_interpolations([$[|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [$[|Buffer], [$]|Search], Output);

extract_interpolations([$]|Rest], Buffer, [$]|Search], Output) ->
  extract_interpolations(Rest, [$]|Buffer], Search, Output);

extract_interpolations([$(|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [$(|Buffer], [$)|Search], Output);

extract_interpolations([$)|Rest], Buffer, [$)|Search], Output) ->
  extract_interpolations(Rest, [$)|Buffer], Search, Output);

% Else

extract_interpolations([Char|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [Char|Buffer], Search, Output).

build_interpol(Piece, [], Output) ->
  Output;

build_interpol(Piece, Buffer, Output) ->
  [{Piece, lists:reverse(Buffer)}|Output].