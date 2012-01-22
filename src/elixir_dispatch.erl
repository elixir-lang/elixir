%% Helpers related to macro dispatch.
-module(elixir_dispatch).
-export([get_functions/1, get_macros/3, format_error/1, dispatch_refer/6, dispatch_imports/5]).
-include("elixir.hrl").

%% Get macros from the given module and raise an
%% exception in case it can't be found

get_functions(Module) ->
  Module:module_info(exports) -- optional_macros(Module).

get_macros(Line, Module, S) ->
  try
    Module:'__info__'(macros)
  catch
    error:undef ->
      Tuple = { no_macros, Module },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

%% Dispatch based on scope's imports

dispatch_imports(Line, Name, Args, S, Callback) ->
  Arity = length(Args),
  case find_dispatch({Name, Arity}, S#elixir_scope.macros) of
    nil ->
      case find_dispatch({Name, Arity}, S#elixir_scope.functions) of
        nil -> Callback();
        Receiver ->
          % elixir_import:record(function, { Name, Arity }, Receiver, S),
          elixir_translator:translate_each({ { '.', Line, [Receiver, Name] }, Line, Args }, S)
      end;
    Receiver ->
      elixir_import:record(macro, { Name, Arity }, Receiver, S),
      dispatch_macro(Line, Receiver, Name, Arity, Args, S)
  end.

%% Dispatch based on scope's refer

dispatch_refer(Line, Receiver, Name, Args, S, Callback) ->
  Arity = length(Args),

  case lists:member({Name, Arity}, optional_macros(Receiver)) of
    true  -> dispatch_macro(Line, Receiver, Name, Arity, Args, S);
    false -> Callback()
  end.

%% HELPERS

optional_macros(Receiver) ->
  try
    Receiver:'__info__'(macros)
  catch
    error:undef -> []
  end.

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
  io_lib:format("tried to use ~s.~s/~B but module was not required. Required: ~p", [Receiver, Name, Arity, Required]);

format_error({ no_macros, Module }) ->
  io_lib:format("could not load macros from module ~s", [Module]).
