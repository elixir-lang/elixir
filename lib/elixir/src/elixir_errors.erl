% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([syntax_error/3, syntax_error/4, inspect/1,
  form_error/4, parse_error/4, assert_module_scope/3,
  assert_no_function_scope/3, assert_function_scope/3,
  assert_no_assign_scope/3, assert_no_guard_scope/3,
  assert_no_assign_or_guard_scope/3,
  handle_file_warning/2, handle_file_warning/3, handle_file_error/2,
  deprecation/3, deprecation/4, file_format/3]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Handle inspecting for exceptions

inspect(Atom) when is_atom(Atom) ->
  case atom_to_list(Atom) of
    "Elixir-" ++ Rest -> [to_dot(R) || R <- Rest];
    Else -> Else
  end;

inspect(Other) -> Other.

to_dot($-) -> $.;
to_dot(L)  -> L.

%% Raised during macros translation.

syntax_error(Line, File, Message) when is_list(Message) ->
  syntax_error(Line, File, iolist_to_binary(Message));

syntax_error(Line, File, Message) when is_binary(Message) ->
  raise(Line, File, 'Elixir.SyntaxError', Message).

syntax_error(Line, File, Format, Args)  ->
  Message = io_lib:format(Format, Args),
  raise(Line, File, 'Elixir.SyntaxError', iolist_to_binary(Message)).

%% Raised on tokenizing/parsing

parse_error(Line, File, Error, []) ->
  Message = case Error of
    "syntax error before: " -> <<"syntax error: expression is incomplete">>;
    _ -> iolist_to_binary(Error)
  end,
  raise(Line, File, 'Elixir.TokenMissingError', Message);

parse_error(Line, File, "syntax error before: ", "'end'") ->
  raise(Line, File, 'Elixir.SyntaxError', <<"unexpected token: end">>);

parse_error(Line, File, Error, Token) ->
  BinError = if
    is_atom(Error) -> atom_to_binary(Error, utf8);
    true -> iolist_to_binary(Error)
  end,

  BinToken = if
    Token == [] -> <<>>;
    true        -> iolist_to_binary(Token)
  end,

  Message = <<BinError / binary, BinToken / binary >>,
  raise(Line, File, 'Elixir.SyntaxError', Message).

%% Raised during compilation

form_error(Line, File, Module, Desc) ->
  Message = iolist_to_binary(format_error(Module, Desc)),
  raise(Line, File, 'Elixir.CompileError', Message).

%% Shows a deprecation message

deprecation(Line, File, Message) -> deprecation(Line, File, Message, []).

deprecation(Line, File, Message, Args) ->
  io:format(file_format(Line, File, io_lib:format(Message, Args))).

%% Handle warnings and errors (called during module compilation)

%% Ignore on bootstrap
handle_file_warning(true, _File, { _Line, sys_core_fold, nomatch_guard }) -> [];
handle_file_warning(true, _File, { _Line, sys_core_fold, { nomatch_shadow, _ } }) -> [];

%% Ignore always
handle_file_warning(_, _File, { _Line, sys_core_fold, useless_building }) -> [];

%% This is an Erlang bug, it considers { tuple, _ }.call to always fail
handle_file_warning(_, _File, { _Line, v3_kernel, bad_call }) -> [];

%% Rewrite
handle_file_warning(_, File, {Line,erl_lint,{undefined_behaviour_func,{Fun,Arity},Module}}) ->
  Raw = "undefined callback function ~s/~B (behaviour ~s)",
  Message = io_lib:format(Raw, [Fun,Arity,inspect(Module)]),
  io:format(file_format(Line, File, Message));

handle_file_warning(_, File, {Line,erl_lint,{undefined_behaviour,Module}}) ->
  Raw = io_lib:format("behaviour ~s undefined", [inspect(Module)]),

  Message = case erlang:function_exported(Module, behavior_info, 1) of
    true  -> Raw ++ " (maybe you meant behaviour_info instead of behavior_info?)";
    false -> Raw
  end,

  io:format(file_format(Line, File, Message));

%% Default behavior
handle_file_warning(_, File, {Line,Module,Desc}) ->
  Message = format_error(Module, Desc),
  io:format(file_format(Line, File, Message)).

handle_file_warning(File, Desc) ->
  handle_file_warning(false, File, Desc).

handle_file_error(File, {Line,Module,Desc}) ->
  form_error(Line, File, Module, Desc).

%% Assertions

assert_no_function_scope(_Line, _Kind, #elixir_scope{function=nil}) -> [];
assert_no_function_scope(Line, Kind, S) ->
  syntax_error(Line, S#elixir_scope.file, "cannot invoke ~s inside a function", [Kind]).

assert_no_assign_or_guard_scope(Line, Kind, S) ->
  assert_no_assign_scope(Line, Kind, S),
  assert_no_guard_scope(Line, Kind, S).

assert_no_assign_scope(Line, Kind, #elixir_scope{context=assign} = S) ->
  syntax_error(Line, S#elixir_scope.file, "cannot invoke ~s inside assign", [Kind]);
assert_no_assign_scope(_Line, _Kind, _S) -> [].

assert_no_guard_scope(Line, Kind, #elixir_scope{context=guard} = S) ->
  syntax_error(Line, S#elixir_scope.file, "cannot invoke ~s inside guard", [Kind]);
assert_no_guard_scope(_Line, _Kind, _S) -> [].

assert_module_scope(Line, Kind, #elixir_scope{module=nil,file=File}) ->
  syntax_error(Line, File, "cannot invoke ~s outside module", [Kind]);
assert_module_scope(_Line, _Kind, #elixir_scope{module=Module}) -> Module.

assert_function_scope(Line, Kind, #elixir_scope{function=nil,file=File}) ->
  syntax_error(Line, File, "cannot invoke ~s outside function", [Kind]);
assert_function_scope(_Line, _Kind, #elixir_scope{function=Function}) -> Function.

%% Helpers

raise(Line, File, Kind, Message) ->
  Stacktrace = erlang:get_stacktrace(),
  erlang:raise(error, { Kind, '__exception__', Message, iolist_to_binary(File), Line }, Stacktrace).

file_format(Line, File, Message) ->
  io_lib:format("~ts:~w: ~ts~n", [File, Line, Message]).

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).