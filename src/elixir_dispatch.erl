%% Helpers related to macro dispatch.
-module(elixir_dispatch).
-export([get_macros/3, format_error/1, dispatch_refer/6, dispatch_imports/5]).
-include("elixir.hrl").

%% Get macros from the given module and raise an
%% exception in case it can't be found

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
  case find_macro({Name, Arity}, S#elixir_scope.imports) of
    nil ->
      Callback();
    Receiver ->
      elixir_import:record(macro, { Name, Arity }, Receiver, S),
      dispatch(Line, Receiver, Name, Arity, Args, S)
  end.

%% Dispatch based on scope's refer

dispatch_refer(Line, Receiver, Name, Args, S, Callback) ->
  Arity  = length(Args),

  %% We are always checking if a macro exist to be
  %% user friendly. That said, if there are no macro,
  %% don't bother, simply skip.
  Macros = try
    Receiver:'__info__'(macros)
  catch
    error:undef -> []
  end,

  case lists:member({Name, Arity}, Macros) of
    true  -> dispatch(Line, Receiver, Name, Arity, Args, S);
    false -> Callback()
  end.

%% HELPERS

dispatch(Line, Receiver, Name, Arity, Args, S) ->
  ensure_required(Line, Receiver, Name, Arity, S),
  Tree = apply(Receiver, Name, Args),
  NewS = S#elixir_scope{macro={Receiver,Name,Arity}},
  { TTree, TS } = elixir_translator:translate_each(Tree, NewS),
  { TTree, TS#elixir_scope{macro=[]} }.

find_macro(Tuple, [{ Name, Values }|T]) ->
  case lists:member(Tuple, Values) of
    true  -> Name;
    false -> find_macro(Tuple, T)
  end;

find_macro(_Tuple, []) -> nil.

%% ERROR HANDLING

ensure_required(Line, Receiver, Name, Arity, S) ->
  Required = [V || { _, V } <- S#elixir_scope.refer],
  case lists:member(Receiver, Required) of
    true  -> ok;
    false ->
      Tuple = { unrequired_module, { Receiver, Name, Arity, Required } },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

format_error({unrequired_module,{Receiver, Name, Arity, Required}}) ->
  io_lib:format("tried to use ~s.~s/~B but module was not required. Required: ~p", [Receiver, Name, Arity, Required]);

format_error({no_macros, Module}) ->
  io_lib:format("could not load macros from module ~s", [Module]).
