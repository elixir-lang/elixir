-module(elixir_errors).
-export([raise/2, raise/3, file_format/3, file_error/4, syntax_error/4]).
-include("elixir.hrl").

raise(Reason, String) ->
  raise(Reason, String, []).

raise(Reason, String, Args) ->
  erlang:error({Reason, lists:flatten(io_lib:format(String, Args))}).

file_format(Line, Filename, Message) ->
  io_lib:format("~ts:~w: ~ts", [Filename, Line, Message]).

file_error(Reason, Line, Filename, Message) ->
  raise(Reason, file_format(Line, Filename, Message)).

syntax_error(Line, Filename, Error, Token) when is_atom(Error) ->
  syntax_error(Line, Filename, [atom_to_list(Error), $\s], Token);

syntax_error(Line, Filename, Error, Token) ->
  file_error(badsyntax, Line, Filename, lists:flatten([Error, Token])).