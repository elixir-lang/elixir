-module(elixir_ref).
-export([last/1, concat/1, lookup/2]).

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