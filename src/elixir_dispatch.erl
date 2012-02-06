%% Helpers related to dispatching to imports.
-module(elixir_dispatch).
-export([get_functions/1, get_macros/3, get_optional_macros/1,
  default_macros/0, default_functions/0, default_refer/0,
  format_error/1, dispatch_refer/6, dispatch_imports/5]).
-include("elixir.hrl").

default_functions() ->
  [
    { '::Elixir::Builtin', get_functions('::Elixir::Builtin') },
    { erlang, in_erlang_functions() }
  ].
default_macros() ->
  [ { '::Elixir::Builtin', get_optional_macros('::Elixir::Builtin') } ].
default_refer() ->
  [ { '::Elixir::Builtin', '::Elixir::Builtin' } ].

%% Get macros/functions from the given module and
%% raise an exception if appropriated.

get_functions('::Elixir::Builtin' = Module) ->
  try
    (Module:module_info(exports) -- get_optional_macros(Module)) --
      [{module_info,0},{module_info,1},{'__info__',1}]
  catch
    error:undef -> []
  end;

get_functions(Module) ->
  Module:module_info(exports) -- get_optional_macros(Module).

get_macros(_Line, '::Elixir::Builtin', _S) ->
  get_optional_macros('::Elixir::Builtin');

get_macros(Line, Module, S) ->
  try
    Module:'__info__'(macros)
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

get_optional_macros('::Elixir::Builtin') ->
  try
    '::Elixir::Builtin':'__info__'(macros) ++ in_erlang_macros()
  catch
    error:undef -> in_erlang_macros()
  end;

get_optional_macros(Receiver) ->
  try
    Receiver:'__info__'(macros)
  catch
    error:undef -> []
  end.

%% Dispatch based on scope's imports

dispatch_imports(Line, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = { Name, Arity },
  case find_dispatch(Tuple, S#elixir_scope.macros) of
    nil ->
      case find_dispatch(Tuple, S#elixir_scope.functions) of
        nil -> Callback();
        erlang -> Callback();
        Receiver ->
          elixir_import:record(import, Tuple, Receiver, S),
          elixir_translator:translate_each({ { '.', Line, [Receiver, Name] }, Line, Args }, S)
      end;
    Receiver ->
      elixir_import:record(import, Tuple, Receiver, S),
      dispatch_macro(Line, Receiver, Tuple, Args, S)
  end.

%% Dispatch based on scope's refer

dispatch_refer(Line, Receiver, Name, Args, S, Callback) ->
  Arity = length(Args),
  Tuple = {Name, Arity},

  case lists:member(Tuple, get_optional_macros(Receiver)) of
    true  -> dispatch_macro(Line, Receiver, Tuple, Args, S);
    false -> Callback()
  end.

%% HELPERS

dispatch_macro(Line, '::Elixir::Builtin' = Receiver, { Name, Arity } = Tuple, Args, S) ->
  case lists:member(Tuple, in_erlang_macros()) of
    true  -> elixir_macros:translate_macro({ Name, Line, Args }, S);
    false -> dispatch_macro(Line, Receiver, Name, Arity, Args, S)
  end;

dispatch_macro(Line, Receiver, { Name, Arity }, Args, S) ->
  dispatch_macro(Line, Receiver, Name, Arity, Args, S).

dispatch_macro(Line, Receiver, Name, Arity, Args, S) ->
  ensure_required(Line, Receiver, Name, Arity, S),
  Tree = try
    apply(Receiver, Name, Args)
  catch
    Kind:Reason ->
      Info = { Receiver, Name, length(Args), [{ file, S#elixir_scope.filename }, { line, Line }] },
      erlang:raise(Kind, Reason, insert_before_dispatch_macro(Info, erlang:get_stacktrace()))
  end,
  NewS = S#elixir_scope{macro={Receiver,Name,Arity}},
  { TTree, TS } = elixir_translator:translate_each(elixir_quote:linify(Line, Tree), NewS),
  { TTree, TS#elixir_scope{macro=[]} }.

find_dispatch(Tuple, [{ Name, Values }|T]) ->
  case lists:member(Tuple, Values) of
    true  -> Name;
    false -> find_dispatch(Tuple, T)
  end;

find_dispatch(_Tuple, []) -> nil.

%% Insert call site into backtrace right after dispatch macro

insert_before_dispatch_macro(Info, [{ elixir_dispatch, dispatch_macro, _, _ }|_] = T) ->
  [Info|T];

insert_before_dispatch_macro(Info, [H|T]) ->
  [H|insert_before_dispatch_macro(Info, T)];

insert_before_dispatch_macro(_, []) ->
  [].

%% ERROR HANDLING

ensure_required(Line, Receiver, Name, Arity, S) ->
  Required = [V || { _, V } <- S#elixir_scope.refer],
  case lists:member(Receiver, Required) of
    true  -> ok;
    false ->
      Tuple = { unrequired_module, { Receiver, Name, Arity, Required } },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

format_error({ unrequired_module,{Receiver, Name, Arity, Required }}) ->
  io_lib:format("tried to invoke macro ~s.~s/~B but module was not required. Required: ~p", [Receiver, Name, Arity, Required]);

format_error({ no_macros, Module }) ->
  io_lib:format("could not load macros from module ~s", [Module]).

%% Implemented in Erlang.

%% TODO: Is there a way to automatically detect all functions
%% automatically imported by Erlang?
in_erlang_functions() ->
  [
    {exit,1},
    {exit,2},
    {error,1},
    {error,2},
    {throw,1}
  ].

in_erlang_macros() ->
  [
    {'@',1},
    {'+',1},
    {'::',1},
    {'+',2},
    {'-',1},
    {'-',2},
    {'*',2},
    {'/',2},
    {'<-',2},
    {'++',2},
    {'--',2},
    {'::',2},
    {'<',2},
    {'>',2},
    {'<=',2},
    {'>=',2},
    {'==',2},
    {'!=',2},
    {'===',2},
    {'!==',2},
    {'apply',3},
    {'andalso',2},
    {'orelse',2},
    {'not',1},
    {'and',2},
    {'or',2},
    {'xor',2},
    {'use',1},
    {'use',2},
    {'defmodule',1},
    {'defmodule',2},
    {'def',1},
    {'def',2},
    {'def',3},
    {'def',4},
    {'defp',1},
    {'defp',2},
    {'defp',3},
    {'defp',4},
    {'defmacro',1},
    {'defmacro',2},
    {'defmacro',3},
    {'defmacro',4},
    {'case',2},
    {'receive',1},
    {'try',1},
    {'var!',1}
  ].