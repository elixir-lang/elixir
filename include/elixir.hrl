-define(ELIXIR_WRAP_CALL(Line, Module, Method, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Method} },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).

-record(elixir_scope, {
  assign=false,                                  %% when true, new variables can be defined in that subtree
  guard=false,                                   %% when true, we are inside a guard
  noref=false,                                   %% when true, don't resolve references
  noname=false,                                  %% when true, don't add new names (used by try)
  check_clauses=true,                            %% when true, check def clauses ordering
  docs=false,                                    %% when true, attach docs to the defined module
  macro=[],                                      %% the current macro being transformed
  function=[],                                   %% the current function
  recur=[],                                      %% the current loop function to be recurred
  module=[],                                     %% the current module
  vars=dict:new(),                               %% a dict of defined variables and their alias
  temp_vars=dict:new(),                          %% a dict of all variables defined in a particular assign
  clause_vars=dict:new(),                        %% a dict of all variables defined in a particular clause
  quote_vars=dict:new(),                         %% a dict of all quoted variables
  counter=0,                                     %% a counter for the variables defined
  filename="nofile",                             %% the current scope filename
  refer=elixir_dispatch:default_refer(),         %% an orddict with references by new -> old names
  macros=elixir_dispatch:default_macros(),       %% a list with macros imported by module
  functions=elixir_dispatch:default_functions(), %% a list with functions imported by module
  scheduled=[]}).                                %% scheduled modules to be loaded

-record(elixir_quote, {
  line=0,
  marker=quoted,
  unquote=true
}).