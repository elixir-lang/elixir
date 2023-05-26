-define(key(M, K), maps:get(K, M)).
-define(ann(Meta), elixir_erl:get_ann(Meta)).
-define(line(Meta), elixir_utils:get_line(Meta)).
-define(generated(Meta), elixir_utils:generated(Meta)).
-define(var_context, ?MODULE).
-define(remote(Ann, Module, Function, Args), {call, Ann, {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}}, Args}).

-record(elixir_ex, {
  caller=false,            %% stores if __CALLER__ is allowed
  %% TODO: Remove warn and everywhere it is set in v2.0
  prematch=warn,           %% {Read, Counter, {bitsize, Original} | none} | warn | raise | pin
  stacktrace=false,        %% stores if __STACKTRACE__ is allowed
  unused={#{}, 0},         %% a map of unused vars and a version counter for vars
  vars={#{}, false}        %% a tuple with maps of read and optional write current vars
}).

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
  terminators=[],
  unescape=true,
  cursor_completion=false,
  existing_atoms_only=false,
  static_atoms_encoder=nil,
  preserve_comments=nil,
  identifier_tokenizer=elixir_tokenizer,
  ascii_identifiers_only=true,
  indentation=0,
  mismatch_hints=[],
  warn_on_unnecessary_quotes=true,
  warnings=[]
}).
