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
%   2) keeps the name of the current method
%   3) keeps the current module kind and name
%   4) keeps a list of defined variables
%   5) keeps a counter with the variables defined
%   6) filename keeps the current scope filename
%   7) keeps a dict of variables that were explictly assigned
%
-record(elixir_scope, {assign=false, method=[], scope={[],[]}, vars=dict:new(), counter=0, filename="nofile", assigned_vars=dict:new()}).