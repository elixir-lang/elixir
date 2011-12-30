% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([syntax_error/4, form_error/4,
  handle_file_warning/2, handle_file_error/2,
  format_error/2]).
-include("elixir.hrl").

syntax_error(Line, Filename, user, Token) ->
  syntax_error(Line, Filename, Token, "");

syntax_error(Line, Filename, Error, Token) ->
  Message = if
    (Token == []) and (Error == "syntax error before: ") -> "syntax error";
    is_atom(Error) -> atom_to_list(Error);
    true -> Error
  end,
  error({badsyntax, {Line, Filename, Message, Token}}).

form_error(Line, Filename, Module, Desc) ->
  error({badform, { Line, Filename, Module, Desc }}).

%% Handle warnings and errors (called during module compilation)

handle_file_warning(Filename, {Line,Module,{unused_var,Var} = Desc}) ->
  case hd(atom_to_list(Var)) == $X of
    true  -> [];
    false -> io:format(file_format(Line, Filename, format_error(Module, Desc)) ++ [$\n])
  end;

handle_file_warning(_Filename, {_Line,sys_core_fold,nomatch_clause_type}) ->
  [];

handle_file_warning(_Filename, {_Line,sys_core_fold,useless_building}) ->
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

file_format(Line, Filename, Message) ->
  lists:flatten(io_lib:format("~ts:~w: ~ts", [Filename, Line, Message])).