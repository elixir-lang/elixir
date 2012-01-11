-module(elixir_ref).
-export([last/1, concat/1, lookup/2,
  format_error/1, ensure_loaded/4]).
-include("elixir.hrl").

%% Ensure a reference is loaded before its usage.

ensure_loaded(Line, Ref, S, Force) ->
  Scheduled = lists:member(Ref, S#elixir_scope.scheduled),
  case not Force andalso Scheduled of
    true  -> ok;
    false ->
      case code:ensure_loaded(Ref) of
        { module, _ }   -> ok;
        { error, What } ->
          Kind = case Scheduled of
            true  -> scheduled_module;
            false -> unloaded_module
          end,
          elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, { Kind, { Ref, What } })
      end
  end.

%% Receives an atom and returns the last reference.

last(Atom) ->
  list_to_atom(last(lists:reverse(atom_to_list(Atom)), [])).

last([$:,$:|_], Acc) -> [$:,$:|Acc];
last([H|T], Acc) -> last(T, [H|Acc]);
last([], Acc) -> Acc.

%% Receives a list of atoms representing modules
%% and concatenate them.

concat(Args) -> list_to_atom(lists:concat([concat_(Arg) || Arg <- Args, Arg /= nil])).

concat_(Arg) ->
  case Ref = atom_to_list(Arg) of
    "::" ++ _ -> Ref;
    _ -> list_to_atom("::" ++ Ref)
  end.

%% Lookup a reference in the current scope

lookup(Else, Dict) ->
  case orddict:find(Else, Dict) of
    { ok, Value } when Value /= Else -> lookup(Value, Dict);
    _ -> Else
  end.

%% Errors

format_error({unloaded_module,{ Module, What }}) ->
  io_lib:format("module ~s is not loaded, reason: ~s", [Module, What]);

format_error({scheduled_module,{ Module, _ }}) ->
  io_lib:format("module ~s is not loaded but was defined. This may happen because the module is nested inside another module. Try defining the module outside the context that requires it.", [Module]).