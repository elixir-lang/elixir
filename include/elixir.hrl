-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).
-define(ELIXIR_ERL_MODULE(Atom), list_to_atom([$e,$x|atom_to_list(Atom)])).
-define(ELIXIR_EX_MODULE(Name), list_to_atom(tl(tl(atom_to_list(Name))))).

% Representation for Elixir Objects. The elixir_module__ holds
% the general represenation and contains:
%
%   1) An Atom 'name that represents the constant assigned to the object
%   2) Data holds the remaining information for the object.
%
-record(elixir_module__, {name=nil, data=[] }).

% Represents an elixir slate. It holds the binding module and data.
-record(elixir_slate__, {module=[], data=[] }).

% Representation for OrderedDict.
-record(elixir_orddict__, {struct=[]}).

% Keeps the scope used in transformation. It contains:
%
%   1)  when true, new variables can be defined in that subtree
%   2)  when true, we are inside a guard
%   3)  when true, don't resolve sub references
%   4)  the current method
%   5)  the current namespace
%   6)  a dict of defined variables and their alias
%   7)  a list of all variables defined in a particular assign
%   8)  a dict of all variables defined in a particular clause
%   9)  a counter with the variables defined
%   10) the current scope filename
%   11) a dict of variables that were explictly assigned and their tree value
%
-record(elixir_scope, {assign=false, guard=false, noref=false, method=[], namespace=[], vars=dict:new(),
  temp_vars=[], clause_vars=dict:new(), counter=0, filename="nofile", assigned_vars=dict:new()}).