-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).

% Representation for Elixir Objects. The elixir_object__ holds
% the general represenation and contains:
%
%   1) An Atom 'name that represents the constant assigned to the object
%   2) An #elixir_object record 'parent that represents the parent object (if it can be represented by a constant)
%   3) A tuple 'mixins containing an ETS table index and a list of mixins
%   4) A tuple 'protos containing an ETS table index and a list of protos
%   5) Data holds the remaining information for the object.
%
-record(elixir_object__, {name=[], parent=[], mixins=[], protos=[], data=[] }).

% Representation for OrderedDict.
-record(elixir_orddict__, {struct=[]}).

% Representation for Strings.
-record(elixir_string__, {struct=[]}).

% Keeps the scope used in transformation. It contains:
%
%   1) when true, new variables can be defined in that subtree
%   2) when true, method declarations are allowed
%   3) keeps the current module name
%   4) filename keeps the current scope filename
%
-record(elixir_scope, {vars=false, method=false, module=[], filename="nofile"}).