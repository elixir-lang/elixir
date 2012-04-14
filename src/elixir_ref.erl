-module(elixir_ref).
-export([last/1, concat/1, lookup/2,
  format_error/1, ensure_loaded/4]).
-include("elixir.hrl").

%% Ensure a reference is loaded before its usage.
ensure_loaded(_Line, '__MAIN__.Elixir.Builtin', _S, _Force) ->
  ok;

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

last([$.|_], Acc) -> "__MAIN__." ++ Acc;
last([H|T], Acc) -> last(T, [H|Acc]);
last([], Acc) -> Acc.

%% Receives a list of atoms representing modules
%% and concatenate them.

concat(Args) ->
  Refs = [concat_(Arg) || Arg <- Args, Arg /= nil],
  list_to_atom(lists:concat(['__MAIN__'|Refs])).

concat_(Arg) when is_binary(Arg) -> concat_(binary_to_list(Arg));
concat_(Arg) when is_atom(Arg) -> concat_(atom_to_list(Arg));
concat_(Arg) when is_list(Arg) ->
  case Arg of
    "__MAIN__" ++ Rest -> Rest;
    "." ++ _ -> Arg;
    _ -> "." ++ Arg
  end.

%% Lookup a reference in the current scope

lookup(Else, Dict) ->
  case orddict:find(Else, Dict) of
    { ok, Value } when Value /= Else -> lookup(Value, Dict);
    _ -> Else
  end.

%% Errors

format_error({unloaded_module,{ Module, What }}) ->
  io_lib:format("module ~s is not loaded, reason: ~s", [elixir_errors:inspect(Module), What]);

format_error({scheduled_module,{ Module, _ }}) ->
  io_lib:format("module ~s is not loaded but was defined. This happens because you are trying to use a module in the same context it is defined. Try defining the module outside the context that requires it.",
    [elixir_errors:inspect(Module)]).