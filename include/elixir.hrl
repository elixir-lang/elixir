-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).
-define(ELIXIR_ERL_MODULE(Atom), list_to_atom([$e,$x|atom_to_list(Atom)])).

% TODO: Fix me
-define(ELIXIR_EX_MODULE(Name), case atom_to_list(Name) of
  [$e,$x|Rest] -> list_to_atom(Rest);
  _ -> Name
end).

% Representation for Elixir Objects. The elixir_object__ holds
% the general represenation and contains:
%
%   1) An Atom 'name that represents the constant assigned to the object
%   2) An #elixir_object record 'parent that represents the parent object (if it can be represented by a constant)
%   3) A tuple 'mixins containing an ETS table index and a list of mixins
%   4) A tuple 'protos containing an ETS table index and a list of protos
%   5) Data holds the remaining information for the object.
%
-record(elixir_object__, {name=nil, parent=[], data=[] }).

% Represents an elixir slate. It holds the binding module and data.
-record(elixir_slate__, {module=[], data=[] }).

% Representation for OrderedDict.
-record(elixir_orddict__, {struct=[]}).

% Keeps the scope used in transformation. It contains:
%
%   1)  when true, new variables can be defined in that subtree
%   2)  when true, we are inside a guard
%   3)  when true, do not assign to the variable name
%   4)  the name of the current method
%   5)  the current module kind and name
%   6)  a dict of defined variables and their alias
%   7)  a list of all variables defined in a particular assign
%   8)  a dict of all variables defined in a particular clause
%   9)  a counter with the variables defined
%   10) the current scope filename
%   11) a dict of variables that were explictly assigned and their tree value
%
-record(elixir_scope, {assign=false, guard=false, noname=false, method=[], scope={[],[]}, vars=dict:new(),
  temp_vars=[], clause_vars=dict:new(), counter=0, filename="nofile", assigned_vars=dict:new()}).