-define(key(M, K), maps:get(K, M)).
-define(ann(Opts), elixir_erl:get_ann(Opts)).
-define(line(Opts), elixir_utils:get_line(Opts)).
-define(generated(Meta), [{generated, true} | Meta]).

-record(elixir_erl, {
  def=nil,                 %% a tuple with the current definition {def | ..., name, arity}
  context=nil,             %% can be match, guards or nil
  extra=nil,               %% extra information about the context, like pin_guard and map_key
  caller=false,            %% when true, it means caller was invoked
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
