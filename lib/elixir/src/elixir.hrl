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
  vars=#{},                %% a map of defined variables and their alias
  backup_vars=nil,         %% a copy of vars to be used on ^var
  extra_guards=[],         %% extra guards from args expansion
  counter=#{},             %% a map counting the variables defined
  stacktrace=false         %% holds information about the stacktrace variable
}).

-record(elixir_quote, {
  line=false,
  file=nil,
  context=nil,
  vars_hygiene=true,
  aliases_hygiene=true,
  imports_hygiene=true,
  unquote=true,
  generated=false
}).

-record(elixir_tokenizer, {
  file=(<<"nofile">>),
  terminators=[],
  unescape=true,
  check_terminators=true,
  existing_atoms_only=false,
  preserve_comments=nil,
  identifier_tokenizer=elixir_tokenizer,
  indentation=0,
  mismatch_hints=[],
  warn_on_unnecessary_quotes=true
}).

%% TODO: Remove this once we support Erlang/OTP 21+ exclusively.
-ifdef(OTP_RELEASE). %% defined on OTP 21+
-define(WITH_STACKTRACE(K, R, S), K:R:S ->).
-else.
-define(WITH_STACKTRACE(K, R, S), K:R -> S = erlang:get_stacktrace(),).
-endif.

%% TODO: Remove this once we support Erlang/OTP 22+ exclusively.
%% See https://github.com/erlang/otp/pull/1972
-ifdef(OTP_RELEASE).
  -if(?OTP_RELEASE >= 22).
    -define(NO_SPAWN_COMPILER_PROCESS, no_spawn_compiler_process).
  -elif(?OTP_RELEASE >= 21).
    -define(NO_SPAWN_COMPILER_PROCESS, dialyzer, no_spawn_compiler_process).
  -endif.
-else.
  -define(NO_SPAWN_COMPILER_PROCESS, dialyzer, no_spawn_compiler_process).
-endif.