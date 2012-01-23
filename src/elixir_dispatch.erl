%% Helpers related to dispatching to imports.
-module(elixir_dispatch).
-export([get_functions/1, get_macros/3, get_optional_macros/1,
  default_macros/0, default_functions/0, default_refer/0,
  format_error/1, dispatch_refer/6, dispatch_imports/5]).
-include("elixir.hrl").

default_functions() ->
  [].
default_macros() ->
  [ { '::Elixir::Macros', get_optional_macros('::Elixir::Macros') } ].
default_refer() ->
  [ { '::Elixir::Macros', '::Elixir::Macros' } ].

%% Get macros/functions from the given module and
%% raise an exception if appropriated.

get_functions(Module) ->
  Module:module_info(exports) -- get_optional_macros(Module).

get_macros(_Line, '::Elixir::Macros', _S) ->
  get_optional_macros('::Elixir::Macros');

get_macros(Line, Module, S) ->
  try
    Module:'__info__'(macros)
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

get_optional_macros('::Elixir::Macros') ->
  try
    '::Elixir::Macros':'__info__'(macros) ++ in_erlang_macros()
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

dispatch_macro(Line, '::Elixir::Macros' = Receiver, { Name, Arity } = Tuple, Args, S) ->
  case lists:member(Tuple, in_erlang_macros()) of
    true  -> elixir_macros:translate_builtin({ Name, Line, Args }, S);
    false -> dispatch_macro(Line, Receiver, Name, Arity, Args, S)
  end;

dispatch_macro(Line, Receiver, { Name, Arity }, Args, S) ->
  dispatch_macro(Line, Receiver, Name, Arity, Args, S).

dispatch_macro(Line, Receiver, Name, Arity, Args, S) ->
  ensure_required(Line, Receiver, Name, Arity, S),
  Tree = apply(Receiver, Name, Args),
  NewS = S#elixir_scope{macro={Receiver,Name,Arity}, line=Line},
  { TTree, TS } = elixir_translator:translate_each(Tree, NewS),
  { TTree, TS#elixir_scope{macro=[],line=[]} }.

find_dispatch(Tuple, [{ Name, Values }|T]) ->
  case lists:member(Tuple, Values) of
    true  -> Name;
    false -> find_dispatch(Tuple, T)
  end;

find_dispatch(_Tuple, []) -> nil.

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

%% Macros implemented in Erlang.

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