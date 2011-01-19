-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ERROR(Atom, String, Args), erlang:error({Atom, lists:flatten(io_lib:format(String, Args))})).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).

% Retrieve values from module names.

-define(ELIXIR_MOD_PROTOS(Name), proplists:get_value(protos, Name:module_info(attributes))).
-define(ELIXIR_MOD_MIXINS(Name), proplists:get_value(mixins, Name:module_info(attributes))).
-define(ELIXIR_MOD_PARENT(Name), case proplists:get_value(parent, Name:module_info(attributes)) of
  []   -> Parent = [];
  Else -> Parent = hd(Else)
end).

% A representation for Elixir Object. It contains:
%
%   1) An Atom 'name that represents the constant assigned to the object
%   2) An #elixir_object record 'parent that represents the parent object (if it can be represented by a constant)
%   3) A tuple 'mixins containing an ETS table index and a list of mixins
%   4) A tuple 'protos containing an ETS table index and a list of protos
%   5) Data holds the remaining information for the object.
%
-record(elixir_object, {name=[], parent=[], mixins=[], protos=[], data=[] }).