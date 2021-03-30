-define(key(M, K), maps:get(K, M)).
-define(ann(Opts), elixir_erl:get_ann(Opts)).
-define(line(Opts), elixir_utils:get_line(Opts)).
-define(generated(Meta), [{generated, true} | Meta]).
-define(var_context, ?MODULE).
-define(remote(Ann, Module, Function, Args), {call, Ann, {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}}, Args}).

-record(elixir_erl, {
  context=nil,             %% can be match, guards or nil
  extra=nil,               %% extra information about the context, like pin_guard and map_key
  caller=false,            %% when true, it means caller was invoked
  var_names=#{},           %% maps of defined variables and their alias
  extra_guards=[],         %% extra guards from args expansion
  counter=#{},             %% a map counting the variables defined
  expand_captures=false,   %% a boolean to control if captures should be expanded
  stacktrace=nil           %% holds information about the stacktrace variable
}).

-record(elixir_tokenizer, {
  file=(<<"nofile">>),
  terminators=[],
  unescape=true,
  check_terminators=true,
  existing_atoms_only=false,
  static_atoms_encoder=nil,
  preserve_comments=nil,
  identifier_tokenizer=elixir_tokenizer,
  indentation=0,
  mismatch_hints=[],
  warn_on_unnecessary_quotes=true,
  warnings=[]
}).
