% A bunch of helpers to help to deal with errors in Elixir source code.
% This is not exposed in the Elixir language.
-module(elixir_errors).
-export([syntax_error/3, syntax_error/4, inspect/1,
  form_error/4, parse_error/4, assert_module_scope/3,
  assert_no_function_scope/3, assert_function_scope/3,
  assert_no_match_scope/3, assert_no_guard_scope/3,
  assert_no_match_or_guard_scope/3,
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

-spec syntax_error(non_neg_integer() | list(), file:filename(), binary() | string()) -> no_return().
-spec syntax_error(non_neg_integer() | list(), file:filename(), binary() | string(), list()) -> no_return().

syntax_error(Meta, File, Message) when is_list(Message) ->
  syntax_error(Meta, File, iolist_to_binary(Message));

syntax_error(Meta, File, Message) when is_binary(Message) ->
  raise(Meta, File, 'Elixir.SyntaxError', Message).

syntax_error(Meta, File, Format, Args)  ->
  Message = io_lib:format(Format, Args),
  raise(Meta, File, 'Elixir.SyntaxError', iolist_to_binary(Message)).

%% Raised on tokenizing/parsing

-spec parse_error(non_neg_integer() | list(), file:filename(), iolist() | atom(), [] | iolist()) -> no_return().

parse_error(Meta, File, Error, []) ->
  Message = case Error of
    "syntax error before: " -> <<"syntax error: expression is incomplete">>;
    _ -> iolist_to_binary(Error)
  end,
  raise(Meta, File, 'Elixir.TokenMissingError', Message);

parse_error(Meta, File, "syntax error before: ", "'end'") ->
  raise(Meta, File, 'Elixir.SyntaxError', <<"unexpected token: end">>);

parse_error(Meta, File, Error, Token) ->
  BinError = if
    is_atom(Error) -> atom_to_binary(Error, utf8);
    true -> iolist_to_binary(Error)
  end,

  BinToken = if
    Token == [] -> <<>>;
    true        -> iolist_to_binary(Token)
  end,

  Message = <<BinError / binary, BinToken / binary >>,
  raise(Meta, File, 'Elixir.SyntaxError', Message).

%% Raised during compilation

-spec form_error(non_neg_integer(), file:filename(), module(), any()) -> no_return().

form_error(Meta, File, Module, Desc) ->
  Message = iolist_to_binary(format_error(Module, Desc)),
  raise(Meta, File, 'Elixir.CompileError', Message).

%% Shows a deprecation message

deprecation(Meta, File, Message) -> deprecation(Meta, File, Message, []).

deprecation(Meta, File, Message, Args) ->
  io:format(file_format(?line(Meta), File, io_lib:format(Message, Args))).

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
  Kind    = protocol_or_behaviour(Module),
  Raw     = "undefined ~ts function ~ts/~B (for ~ts ~ts)",
  Message = io_lib:format(Raw, [Kind, Fun, Arity, Kind, inspect(Module)]),
  io:format(file_format(Line, File, Message));

handle_file_warning(_, File, {Line,erl_lint,{undefined_behaviour,Module}}) ->
  case elixir_compiler:get_opt(internal) of
    true  -> [];
    false ->
      Message = io_lib:format("behaviour ~ts undefined", [inspect(Module)]),
      io:format(file_format(Line, File, Message))
  end;

handle_file_warning(_, _File, {Line,erl_lint,{unused_var,_Var}}) when Line =< 0 ->
  [];

handle_file_warning(_, File, {Line,erl_lint,{unused_var,Var}}) ->
  Message = format_error(erl_lint, { unused_var, format_var(Var) }),
  io:format(file_format(Line, File, Message));

handle_file_warning(_, File, {Line,erl_lint,{shadowed_var,Var,Where}}) ->
  Message = format_error(erl_lint, { shadowed_var, format_var(Var), Where }),
  io:format(file_format(Line, File, Message));

%% Default behavior
handle_file_warning(_, File, {Line,Module,Desc}) ->
  Message = format_error(Module, Desc),
  io:format(file_format(Line, File, Message)).

handle_file_warning(File, Desc) ->
  handle_file_warning(false, File, Desc).

-spec handle_file_error(file:filename(), {non_neg_integer(), module(), any()}) -> no_return().

handle_file_error(File, {Line,Module,Desc}) ->
  form_error(Line, File, Module, Desc).

%% Assertions

assert_no_function_scope(_Meta, _Kind, #elixir_scope{function=nil}) -> [];
assert_no_function_scope(Meta, Kind, S) ->
  syntax_error(Meta, S#elixir_scope.file, "cannot invoke ~ts inside a function", [Kind]).

assert_no_match_or_guard_scope(Meta, Kind, S) ->
  assert_no_match_scope(Meta, Kind, S),
  assert_no_guard_scope(Meta, Kind, S).

assert_no_match_scope(Meta, Kind, #elixir_scope{context=match} = S) ->
  syntax_error(Meta, S#elixir_scope.file, "cannot invoke ~ts inside match clause", [Kind]);
assert_no_match_scope(_Meta, _Kind, _S) -> [].

assert_no_guard_scope(Meta, Kind, #elixir_scope{context=guard} = S) ->
  syntax_error(Meta, S#elixir_scope.file, "cannot invoke ~ts inside guard", [Kind]);
assert_no_guard_scope(_Meta, _Kind, _S) -> [].

assert_module_scope(Meta, Kind, #elixir_scope{module=nil,file=File}) ->
  syntax_error(Meta, File, "cannot invoke ~ts outside module", [Kind]);
assert_module_scope(_Meta, _Kind, #elixir_scope{module=Module}) -> Module.

assert_function_scope(Meta, Kind, #elixir_scope{function=nil,file=File}) ->
  syntax_error(Meta, File, "cannot invoke ~ts outside function", [Kind]);
assert_function_scope(_Meta, _Kind, #elixir_scope{function=Function}) -> Function.

%% Helpers

raise(Meta, File, Kind, Message) when is_list(Meta) ->
  raise(?line(Meta), File, Kind, Message);

raise(none, File, Kind, Message) ->
  raise(0, File, Kind, Message);

raise(Line, File, Kind, Message) when is_integer(Line) ->
  Stacktrace = erlang:get_stacktrace(),
  Exception = Kind:new([{description, Message}, {file, iolist_to_binary(File)}, {line, Line}]),
  erlang:raise(error, Exception, Stacktrace).

file_format(0, File, Message) ->
  io_lib:format("~ts: ~ts~n", [File, Message]);

file_format(Line, File, Message) ->
  io_lib:format("~ts:~w: ~ts~n", [File, Line, Message]).

format_var(Var) ->
  list_to_atom(lists:takewhile(fun(X) -> X /= $@ end, atom_to_list(Var))).

format_error([], Desc) ->
  io_lib:format("~p", [Desc]);

format_error(Module, Desc) ->
  Module:format_error(Desc).

protocol_or_behaviour(Module) ->
  case is_protocol(Module) of
    true  -> protocol;
    false -> behaviour
  end.

is_protocol(Module) ->
  case code:ensure_loaded(Module) of
    { module, _ } ->
      case erlang:function_exported(Module, '__protocol__', 1) of
        true  -> Module:'__protocol__'(name) == Module;
        false -> false
      end;
    { error, _ } ->
      false
  end.