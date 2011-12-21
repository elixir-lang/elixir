-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).

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
%
-record(elixir_scope, {assign=false, guard=false, noref=false, method=[], namespace=[], vars=dict:new(),
  temp_vars=[], clause_vars=dict:new(), counter=0, filename="nofile"}).