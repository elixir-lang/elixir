-module(elixir_ref).
-export([concat/1]).

%% Receives a list of atoms representing modules
%% and concatenate them.
concat(Args) -> list_to_atom(lists:concat([concat_(Arg) || Arg <- Args, Arg /= nil])).

concat_(Arg) ->
  case Ref = atom_to_list(Arg) of
    "::" ++ _ -> Ref;
    _ -> list_to_atom("::" ++ Ref)
  end.

