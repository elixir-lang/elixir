-define(wrap_call(Line, Module, Function, Args),
  { call, Line,
    { remote, Line, { atom, Line, Module }, { atom, Line, Function } },
    Args
  }).

-define(atom_concat(Atoms), list_to_atom(lists:concat(Atoms))).
-define(elixir_macro(Macro), list_to_atom(lists:concat(['MACRO-',Macro]))).
-define(line(Opts), elixir_tree_helpers:get_line(Opts)).

-record(elixir_scope, {
  context=nil,             %% can be assign, guards or nil
  noname=false,            %% when true, don't add new names (used by try)
  check_clauses=true,      %% when true, check def clauses ordering
  super=false,             %% when true, it means super was invoked
  caller=false,            %% when true, it means caller was invoked
  name_args=false,         %% when true, it means arguments should be named
  module=nil,              %% the current module
  function=nil,            %% the current function
  vars=[],                 %% a dict of defined variables and their alias
  temp_vars=[],            %% a dict of all variables defined in a particular assign
  clause_vars=nil,         %% a dict of all variables defined in a particular clause
  extra_guards=nil,        %% extra guards from args expansion
  counter=[],              %% a counter for the variables defined
  local=nil,               %% the scope to evaluate local functions against
  scheduled=[],            %% scheduled modules to be loaded
  macro_aliases=[],        %% keep aliases defined inside a macro
  aliases,                 %% an orddict with aliases by new -> old names
  file,                    %% the current scope filename
  requires,                %% a set with modules required
  macro_macros=[],         %% a list with macros imported from module inside a macro
  macros,                  %% a list with macros imported from module
  macro_functions=[],      %% a list with functions imported from module inside a macro
  functions                %% a list with functions imported from module
}).

-record(elixir_quote, {
  line=0,
  vars_hygiene=nil,
  aliases_hygiene=true,
  imports_hygiene=true,
  unquote=true
}).

%% Used in tokenization and interpolation

-define(is_digit(S), S >= $0 andalso S =< $9).
-define(is_hex(S), ?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f)).
-define(is_bin(S), S >= $0 andalso S =< $1).
-define(is_octal(S), S >= $0 andalso S =< $7).
-define(is_leading_octal(S), S >= $0 andalso S =< $3).
-define(is_upcase(S), S >= $A andalso S =< $Z).
-define(is_downcase(S), S >= $a andalso S =< $z).
-define(is_word(S), ?is_digit(S) orelse ?is_upcase(S) orelse ?is_downcase(S)).
-define(is_quote(S), S == $" orelse S == $').
-define(is_space(S), S == $\s; S == $\r; S == $\t; S == $\n).
