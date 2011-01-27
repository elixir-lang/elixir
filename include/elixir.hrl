-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).

-define(ELIXIR_ERROR(Atom, String, Args), erlang:error({Atom, lists:flatten(io_lib:format(String, Args))})).

-define(ELIXIR_SYNTAX_ERROR(Line, Filename, Error, Token), 
  ?ELIXIR_ERROR(badsyntax, "~s:~w: ~s", [Filename, Line, lists:flatten([Error, Token])])
).

% A representation for Elixir Object. It contains:
%
%   1) An Atom 'name that represents the constant assigned to the object
%   2) An #elixir_object record 'parent that represents the parent object (if it can be represented by a constant)
%   3) A tuple 'mixins containing an ETS table index and a list of mixins
%   4) A tuple 'protos containing an ETS table index and a list of protos
%   5) Data holds the remaining information for the object.
%
-record(elixir_object, {name=[], parent=[], mixins=[], protos=[], data=[] }).