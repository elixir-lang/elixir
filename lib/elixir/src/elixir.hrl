-define(m(M, K), maps:get(K, M)).
-define(ann(Opts), elixir_utils:get_ann(Opts)).
-define(line(Opts), elixir_utils:get_line(Opts)).
-define(generated, [{generated, true}, {location, 0}]).

-record(elixir_scope, {
  context=nil,             %% can be match, guards or nil
  extra=nil,               %% extra information about the context, like pin_guard and map_key
  super=false,             %% when true, it means super was invoked
  caller=false,            %% when true, it means caller was invoked
  module=nil,              %% the current module
  function=nil,            %% the current function
  vars=#{},                %% a map of defined variables and their alias
  backup_vars=nil,         %% a copy of vars to be used on ^var
  match_vars=nil,          %% a set of all variables defined in a particular match
  export_vars=nil,         %% a dict of all variables defined in a particular clause
  extra_guards=nil,        %% extra guards from args expansion
  counter=#{},             %% a map counting the variables defined
  file=(<<"nofile">>)      %% the current scope filename
}).

-record(elixir_quote, {
  line=false,
  file=nil,
  context=nil,
  vars_hygiene=true,
  aliases_hygiene=true,
  imports_hygiene=true,
  unquote=true,
  unquoted=false,
  escape=false,
  generated=false
}).

-record(elixir_tokenizer, {
  file,
  terminators=[],
  check_terminators=true,
  existing_atoms_only=false
}).

%% Used in tokenization and interpolation

%% Numbers
-define(is_hex(S), (?is_digit(S) orelse (S >= $A andalso S =< $F) orelse (S >= $a andalso S =< $f))).
-define(is_bin(S), (S >= $0 andalso S =< $1)).
-define(is_octal(S), (S >= $0 andalso S =< $7)).
-define(is_leading_octal(S), (S >= $0 andalso S =< $3)).

%% Digits and letters
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).

%% Atoms
-define(is_atom_start(S), (?is_quote(S) orelse ?is_upcase(S) orelse ?is_downcase(S) orelse (S == $_))).
-define(is_atom(S), (?is_identifier(S) orelse (S == $@))).

-define(is_identifier_start(S), (?is_upcase(S) orelse ?is_downcase(S) orelse (S == $_))).
-define(is_identifier(S), (?is_digit(S) orelse ?is_identifier_start(S))).
-define(is_sigil(S), ((S == $/) orelse (S == $<) orelse (S == $") orelse (S == $') orelse
                      (S == $[) orelse (S == $() orelse (S == ${) orelse (S == $|))).

%% Quotes
-define(is_quote(S), (S == $" orelse S == $')).

%% Spaces
-define(is_horizontal_space(S), ((S == $\s) orelse (S == $\t))).
-define(is_vertical_space(S), ((S == $\r) orelse (S == $\n))).
-define(is_space(S), (?is_horizontal_space(S) orelse ?is_vertical_space(S))).
