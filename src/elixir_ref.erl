-module(elixir_ref).
-export([last/1, concat/1, safe_concat/1, lookup/2,
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
      try
        Ref:module_info(compile)
      catch
        error:undef ->
          Kind = case Scheduled of
            true  -> scheduled_module;
            false -> unloaded_module
          end,
          elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, { Kind, Ref })
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

concat(Args) -> list_to_atom(raw_concat(Args)).
safe_concat(Args) -> list_to_existing_atom(raw_concat(Args)).

raw_concat(Args) ->
  Refs = [to_partial_ref(Arg) || Arg <- Args, Arg /= nil],
  [$_, $_, $M, $A, $I, $N, $_, $_ | lists:concat(Refs)].

to_partial_ref(Arg) when is_binary(Arg) -> to_partial_ref(binary_to_list(Arg));
to_partial_ref(Arg) when is_atom(Arg)   -> to_partial_ref(atom_to_list(Arg));
to_partial_ref("__MAIN__" ++ Arg)       -> Arg;
to_partial_ref([$.|_] = Arg)            -> Arg;
to_partial_ref(Arg) when is_list(Arg)   -> [$.|Arg].

%% Lookup a reference in the current scope

lookup(Else, Dict) ->
  case orddict:find(Else, Dict) of
    { ok, Value } when Value /= Else -> lookup(Value, Dict);
    _ -> Else
  end.

%% Errors

format_error({unloaded_module, Module}) ->
  io_lib:format("module ~s is not loaded and could not be found", [elixir_errors:inspect(Module)]);

format_error({scheduled_module, Module}) ->
  io_lib:format("module ~s is not loaded but was defined. This happens because you are trying to use a module in the same context it is defined. Try defining the module outside the context that requires it.",
    [elixir_errors:inspect(Module)]).