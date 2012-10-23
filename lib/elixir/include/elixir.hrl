-define(ELIXIR_WRAP_CALL(Line, Module, Function, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Function } },
    Args
  }).

-define(ELIXIR_ATOM_CONCAT(Atoms), list_to_atom(lists:concat(Atoms))).
-define(ELIXIR_MACRO(Macro), list_to_atom(lists:concat(['MACRO-',Macro]))).

-record(elixir_scope, {
  context=nil,                                   %% can be assign, guards or nil
  noname=false,                                  %% when true, don't add new names (used by try)
  check_clauses=true,                            %% when true, check def clauses ordering
  super=false,                                   %% when true, it means super was invoked
  caller=false,                                  %% when true, it means caller was invoked
  name_args=false,                               %% when true, it means arguments should be named
  macro=[],                                      %% a stack with macros nesting
  module=nil,                                    %% the current module
  function=nil,                                  %% the current function
  recur=nil,                                     %% the current loop function to be recurred
  vars=[],                                       %% a dict of defined variables and their alias
  temp_vars=[],                                  %% a dict of all variables defined in a particular assign
  quote_vars=[],                                 %% a dict of all quoted variables
  clause_vars=nil,                               %% a dict of all variables defined in a particular clause
  extra_guards=nil,                              %% extra guards from args expansion
  counter=0,                                     %% a counter for the variables defined
  file=(<<"nofile">>),                           %% the current scope filename
  local=nil,                                     %% the scope to evaluate local functions against
  aliases=[],                                    %% an orddict with aliases by new -> old names
  requires=elixir_dispatch:default_requires(),   %% a set with modules required
  macros=elixir_dispatch:default_macros(),       %% a list with macros imported by module
  functions=elixir_dispatch:default_functions(), %% a list with functions imported by module
  scheduled=[]}).                                %% scheduled modules to be loaded

-record(elixir_quote, {
  line=0,
  marker=quoted,
  unquote=true
}).