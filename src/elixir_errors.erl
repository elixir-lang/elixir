% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([syntax_error/4, form_error/4,
  handle_file_warning/2, handle_file_error/2,
  format_error/2]).
-include("elixir.hrl").

syntax_error(Line, Filename, user, Token) ->
  syntax_error(Line, Filename, Token, "");

syntax_error(Line, Filename, _Error, []) ->
  raise(Line, Filename, '::TokenMissingError', <<"syntax error: expression is incomplete">>);

syntax_error(Line, Filename, Error, Token) ->
  BinError = if
    (Token == []) and (Error == "syntax error before: ") -> <<"syntax error">>;
    is_atom(Error) -> atom_to_binary(Error, utf8);
    true -> iolist_to_binary(Error)
  end,

  BinToken = case Token of
    [] -> <<>>;
    _  -> iolist_to_binary(Token)
  end,

  Message = <<BinError / binary, BinToken / binary >>,
  raise(Line, Filename, '::SyntaxError', Message).

form_error(Line, Filename, Module, Desc) ->
  Message = iolist_to_binary(format_error(Module, Desc)),
  raise(Line, Filename, '::CompileError', Message).

%% Handle warnings and errors (called during module compilation)

handle_file_warning(_Filename, {_Line,sys_core_fold,Ignore}) when
  Ignore == nomatch_clause_type; Ignore == useless_building ->
  [];

handle_file_warning(Filename, {Line,Module,Desc}) ->
  Message = format_error(Module, Desc),
  io:format(file_format(Line, Filename, Message) ++ "\n").

handle_file_error(Filename, {Line,Module,Desc}) ->
  form_error(Line, Filename, Module, Desc).

%% Format each error or warning in the format { Line, Module, Desc }

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).

%% Helpers

raise(Line, Filename, Kind, Message) ->
  Stacktrace0 = erlang:get_stacktrace(),
  Stacktrace1 = [{ elixir_errors, raise, 4, [{file, Filename},{line, Line}]} | Stacktrace0],
  erlang:raise(error, { Kind, '__EXCEPTION__', Message }, Stacktrace1).

file_format(Line, Filename, Message) ->
  lists:flatten(io_lib:format("~ts:~w: ~ts", [Filename, Line, Message])).