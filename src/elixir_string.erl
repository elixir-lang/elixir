-module(elixir_string).
-export([stringify/1, extract_interpolations/1]).
-include("elixir.hrl").

% TODO Remove stringify once we dispatch contacatenation results to to_s
stringify(#elixir_object{parent='String', data=Data}) ->
  hd(dict:fetch(list, Data));

stringify(Arg) ->
  io_lib:format("~ts", [Arg]).

extract_interpolations(String) ->
  extract_interpolations(String, [], [], []).

extract_interpolations([], Buffer, [], Output) ->
  lists:reverse(build_interpol(s, Buffer, Output));

extract_interpolations([], Buffer, Search, Output) ->
  ?ELIXIR_ERROR(badarg, "Unexpected end of string, expected ~s", [[hd(Search)]]);

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

extract_interpolations([$}|Rest], Buffer, [$}|Search], Output) ->
  extract_interpolations(Rest, [$}|Buffer], Search, Output);

extract_interpolations([${|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [${|Buffer], [$}|Search], Output);

extract_interpolations([$"|Rest], Buffer, [$"|Search], Output) ->
  extract_interpolations(Rest, [$"|Buffer], Search, Output);

extract_interpolations([$"|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [$"|Buffer], [$"|Search], Output);

extract_interpolations([Char|Rest], Buffer, Search, Output) ->
  extract_interpolations(Rest, [Char|Buffer], Search, Output).

build_interpol(Piece, [], Output) ->
  Output;

build_interpol(Piece, Buffer, Output) ->
  [{Piece, lists:reverse(Buffer)}|Output].