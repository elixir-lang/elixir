% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([error/1, file_format/3, syntax_error/3, syntax_error/4, handle_file_warning/2,
  handle_file_error/2, format_error/2, format_error/1]).
-include("elixir.hrl").

error(Reason) -> erlang:error(Reason).

file_format(Line, Filename, Message) ->
  lists:flatten(io_lib:format("~ts:~w: ~ts", [Filename, Line, Message])).

syntax_error(Line, Filename, Token) ->
  syntax_error(Line, Filename, Token, "").

syntax_error(Line, Filename, user, Token) ->
  syntax_error(Line, Filename, Token, "");

syntax_error(Line, Filename, Error, Token) ->
  Message = if
    (Token == []) and (Error == "syntax error before:") -> <<"syntax error">>;
    is_atom(Error) -> atom_to_binary(Error, utf8);
    true -> iolist_to_binary(Error)
  end,
  elixir_errors:error({badsyntax, {Line, iolist_to_binary(Filename), Message, iolist_to_binary(Token)}}).

% Handle warnings

handle_file_warning(Filename, {Line,Module,{unused_var,Var} = Desc}) ->
  case hd(atom_to_list(Var)) == $X of
    true  -> [];
    false -> io:format(file_format(Line, Filename, format_error(Module, Desc)) ++ [$\n])
  end;

handle_file_warning(Filename, {Line,Module,Desc}) ->
  Message = format_error(Module, Desc),
  io:format(file_format(Line, Filename, Message) ++ [$\n]).

% Handle errors

handle_file_error(Filename, {Line,Module,Desc}) ->
  elixir_errors:error({badform, { Line, list_to_binary(Filename), Module, Desc }}).

% Format each error or warning in the format { Line, Module, Desc }

format_error(_, {changed_visibility,{Name,Arity,Previous}}) ->
  io_lib:format("function ~s/~B already defined with visibility ~s", [Name, Arity, Previous]);

format_error(_, {changed_kind,{Name,Arity,Previous}}) ->
  io_lib:format("function ~s/~B already defined as ~s", [Name, Arity, Previous]);

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).

format_error(Desc) -> format_error([], Desc).