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
%   4)  when true, don't add new names. used by try.
%   5)  the current macro being transformed
%   6)  the current function
%   7)  the current module
%   8)  a dict of defined variables and their alias
%   9)  a list of all variables defined in a particular assign
%   10) a dict of all variables defined in a particular clause
%   11) a counter for the variables defined
%   12) the current scope filename
%   13) reference variables
%
-record(elixir_scope, {assign=false, guard=false, noref=false, noname=false, macro=[], function=[],
  module={0,nil}, vars=dict:new(), temp_vars=[], clause_vars=dict:new(), counter=0,
  filename="nofile", refer=[]}).