-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).

% A representation for Elixir Object. It containts:
%
%   1) An Atom 'name that represents the constant assigned to the object
%   2) An Atom 'parent that represents the parent object (if it can be represented by a constant)
%   3) A tuple 'mixin containing an ETS table index and a list of mixins
%   4) A tuple 'proto containing an ETS table index and a list of protos
%   5) A tuple of pairs containing info for instance variable lookup
%
-record(elixir_object, {name=[], parent=[], mixin=[], proto=[], data=[] }).