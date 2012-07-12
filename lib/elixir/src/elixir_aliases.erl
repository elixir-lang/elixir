-module(elixir_aliases).
-export([first/1, last/1, concat/1, safe_concat/1, lookup/2,
  format_error/1, ensure_loaded/3]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

%% Ensure a module is loaded before its usage.
ensure_loaded(_Line, 'Elixir.Elixir.Builtin', _S) ->
  ok;

ensure_loaded(Line, Ref, S) ->
  try
    Ref:module_info(compile)
  catch
    error:undef ->
      Kind = case lists:member(Ref, S#elixir_scope.scheduled) of
        true  -> scheduled_module;
        false -> unloaded_module
      end,
      elixir_errors:form_error(Line, S#elixir_scope.file, ?MODULE, { Kind, Ref })
  end.

%% Receives an atom and returns the first alias.

first(Atom) ->
  First = first(atom_to_list(Atom), []),
  list_to_atom("__MAIN__-" ++ First).

first("__MAIN__-" ++ Rest, []) -> first(Rest, []);
first([$-|_], Acc) -> lists:reverse(Acc);
first([H|T], Acc) -> first(T, [H|Acc]);
first([], Acc) -> lists:reverse(Acc).

%% Receives an atom and returns the last alias.

last(Atom) ->
  Last = last(lists:reverse(atom_to_list(Atom)), []),
  list_to_atom("__MAIN__-" ++ Last).

last([$-|_], Acc) -> Acc;
last([H|T], Acc) -> last(T, [H|Acc]);
last([], Acc) -> Acc.

%% Receives a list of atoms representing modules
%% and concatenate them.

concat(Args) -> list_to_atom(raw_concat(Args)).
safe_concat(Args) -> list_to_existing_atom(raw_concat(Args)).

raw_concat(Args) ->
  Aliases = [to_partial(Arg) || Arg <- Args, Arg /= nil],
  "__MAIN__" ++ lists:concat(Aliases).

to_partial(Arg) when is_binary(Arg) -> to_partial(binary_to_list(Arg));
to_partial(Arg) when is_atom(Arg)   -> to_partial(atom_to_list(Arg));
to_partial("__MAIN__" ++ Arg)       -> Arg;
to_partial([$-|_] = Arg)            -> Arg;
to_partial(Arg) when is_list(Arg)   -> [$-|Arg].

%% Lookup an alias in the current scope

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