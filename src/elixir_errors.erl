% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([syntax_error/3, syntax_error/4,
  form_error/4, parse_error/4, assert_module_scope/3,
  assert_no_function_scope/3, assert_function_scope/3,
  handle_file_warning/2, handle_file_error/2]).
-include("elixir.hrl").

%% Raised during macros translation.

syntax_error(Line, Filename, Message) when is_list(Message) ->
  syntax_error(Line, Filename, iolist_to_binary(Message));

syntax_error(Line, Filename, Message) when is_binary(Message) ->
  raise(Line, Filename, '::SyntaxError', Message).

syntax_error(Line, Filename, Format, Args)  ->
  Message = io_lib:format(Format, Args),
  raise(Line, Filename, '::SyntaxError', iolist_to_binary(Message)).

%% Raised on tokenizing/parsing

parse_error(Line, Filename, _Error, []) ->
  raise(Line, Filename, '::TokenMissingError', <<"syntax error: expression is incomplete">>);

parse_error(Line, Filename, Error, Token) ->
  BinError = if
    is_atom(Error) -> atom_to_binary(Error, utf8);
    true -> iolist_to_binary(Error)
  end,

  BinToken = case Token of
    [] -> <<>>;
    _  -> iolist_to_binary(Token)
  end,

  Message = <<BinError / binary, BinToken / binary >>,
  raise(Line, Filename, '::SyntaxError', Message).

%% Raised during compilation

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

%% Assertions

assert_no_function_scope(_Line, _Kind, #elixir_scope{function=[]}) -> [];
assert_no_function_scope(Line, Kind, S) ->
  syntax_error(Line, S#elixir_scope.filename, "cannot invoke ~s inside a function", [Kind]).

assert_module_scope(Line, Kind, #elixir_scope{module=[],filename=Filename}) ->
  syntax_error(Line, Filename, "cannot invoke ~s outside module", [Kind]);
assert_module_scope(_Line, _Kind, #elixir_scope{module=Module}) -> Module.

assert_function_scope(Line, Kind, #elixir_scope{function=[],filename=Filename}) ->
  syntax_error(Line, Filename, "cannot invoke ~s outside function", [Kind]);
assert_function_scope(_Line, _Kind, #elixir_scope{function=Function}) -> Function.

%% Helpers

raise(Line, Filename, Kind, Message) ->
  Stacktrace = erlang:get_stacktrace(),
  erlang:raise(error, { Kind, '__EXCEPTION__', Message, iolist_to_binary(Filename), Line }, Stacktrace).

file_format(Line, Filename, Message) ->
  lists:flatten(io_lib:format("~ts:~w: ~ts", [Filename, Line, Message])).

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).