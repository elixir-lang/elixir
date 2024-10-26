-define(key(M, K), map_get(K, M)).
-define(ann(Meta), elixir_erl:get_ann(Meta)).
-define(line(Meta), elixir_utils:get_line(Meta)).
-define(generated(Meta), elixir_utils:generated(Meta)).
-define(var_context, ?MODULE).
-define(remote(Ann, Module, Function, Args), {call, Ann, {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}}, Args}).

-record(elixir_ex, {
  %% Stores if __CALLER__ is allowed
  caller=false,
  %% Stores the variables available before a match.
  %% May be one of:
  %%
  %%   * {Read, Cycle :: #{}, Meta :: Counter | {bitsize, Original}}
  %%   * pin
  %%   * none.
  %%
  %% The cycle is used to detect cyclic dependencies between
  %% variables in a match.
  %%
  %% The bitsize is used when dealing with bitstring modifiers,
  %% as they allow guards but also support the pin operator.
  prematch=none,
  %% Stores if __STACKTRACE__ is allowed
  stacktrace=false,
  %% A map of unused vars and a version counter for vars
  unused={#{}, 0},
  %% A list of modules defined in functions (runtime)
  runtime_modules=[],
  %% A tuple with maps of read and optional write current vars.
  %% Read variables is all defined variables. Write variables
  %% stores the variables that have been made available (written
  %% to) but cannot be currently read. This is used in two occasions:
  %%
  %%   * To store variables graphs inside = in patterns
  %%
  %%   * To store variables defined inside calls. For example,
  %%     if you write foo(a = 123), the value of `a` cannot be
  %%     read in the following argument, only after the call
  %%
  vars={#{}, false}
}).

-record(elixir_erl, {
  %% Can be match, guards or nil
  context=nil,
  %% Extra information about the context, like pin_guard and map_key
  extra=nil,
  %% When true, it means caller was invoked
  caller=false,
  %% Maps of defined variables and their alias
  var_names=#{},
  %% Extra guards from args expansion
  extra_guards=[],
  %% A map counting the variables defined
  counter=#{},
  %% A boolean to control if captures should be expanded
  expand_captures=false,
  %% Holds information about the stacktrace variable
  stacktrace=nil
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
  column=1,
  mismatch_hints=[],
  warnings=[]
}).
