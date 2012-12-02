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
  module=nil,                                    %% the current module
  function=nil,                                  %% the current function
  vars=[],                                       %% a dict of defined variables and their alias
  temp_vars=[],                                  %% a dict of all variables defined in a particular assign
  clause_vars=nil,                               %% a dict of all variables defined in a particular clause
  extra_guards=nil,                              %% extra guards from args expansion
  counter=[],                                    %% a counter for the variables defined
  local=nil,                                     %% the scope to evaluate local functions against
  scheduled=[],                                  %% scheduled modules to be loaded
  file,                                          %% the current scope filename
  aliases,                                       %% an orddict with aliases by new -> old names
  requires,                                      %% a set with modules required
  macros,                                        %% a list with macros imported by module
  functions}).                                   %% a list with functions imported by module

-record(elixir_quote, {
  line=0,
  marker=quoted,
  unquote=true
}).