-module(elixir_code_server).
-export([call/1, cast/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-define(timeout, 30000).
-record(elixir_code_server, {
  required=#{},
  mod_pool={[], [], 0},
  mod_ets=#{},
  compilation_status=#{}
}).

call(Args) ->
  gen_server:call(?MODULE, Args, ?timeout).

cast(Args) ->
  gen_server:cast(?MODULE, Args).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).

init(ok) ->
  %% The table where we store module definitions
  _ = ets:new(elixir_modules, [set, protected, named_table, {read_concurrency, true}]),
  {ok, #elixir_code_server{}}.

handle_call({defmodule, Module, Pid, Tuple}, _From, Config) ->
  case ets:lookup(elixir_modules, Module) of
    [] ->
      {Ref, NewConfig} = defmodule(Pid, Tuple, Config),
      {reply, {ok, Ref}, NewConfig};
    [CurrentTuple] ->
      {reply, {error, CurrentTuple}, Config}
  end;

handle_call({undefmodule, Ref}, _From, Config) ->
  {reply, ok, undefmodule(Ref, Config)};

handle_call({acquire, Path}, From, Config) ->
  Current = Config#elixir_code_server.required,
  case maps:find(Path, Current) of
    {ok, true} ->
      {reply, required, Config};
    {ok, Queued} when is_list(Queued) ->
      Required = maps:put(Path, [From | Queued], Current),
      {noreply, Config#elixir_code_server{required=Required}};
    error ->
      Required = maps:put(Path, [], Current),
      {reply, proceed, Config#elixir_code_server{required=Required}}
  end;

handle_call(required, _From, Config) ->
  {reply, [F || {F, true} <- maps:to_list(Config#elixir_code_server.required)], Config};

handle_call({compilation_status, CompilerPid}, _From, Config) ->
  CompilationStatusList = Config#elixir_code_server.compilation_status,
  CompilationStatusListNew = maps:remove(CompilerPid, CompilationStatusList),
  CompilationStatus = maps:get(CompilerPid, CompilationStatusList),
  {reply, CompilationStatus,
   Config#elixir_code_server{compilation_status=CompilationStatusListNew}};

handle_call(retrieve_compiler_module, _From, Config) ->
  case Config#elixir_code_server.mod_pool of
    {Used, [Mod | Unused], Counter} ->
      {reply, Mod, Config#elixir_code_server{mod_pool={Used, Unused, Counter}}};
    {Used, [], Counter} ->
      {reply, compiler_module(Counter), Config#elixir_code_server{mod_pool={Used, [], Counter+1}}}
  end;

handle_call(purge_compiler_modules, _From, Config) ->
  {Used, Unused, Counter} = Config#elixir_code_server.mod_pool,
  _ = [code:purge(Module) || Module <- Used],
  ModPool = {[], Used ++ Unused, Counter},
  {reply, {ok, length(Used)}, Config#elixir_code_server{mod_pool=ModPool}};

handle_call(Request, _From, Config) ->
  {stop, {badcall, Request}, Config}.

handle_cast({register_warning, CompilerPid}, Config) ->
  CompilationStatusCurrent = Config#elixir_code_server.compilation_status,
  CompilationStatusNew = maps:put(CompilerPid, error, CompilationStatusCurrent),
  CompilerOptions = elixir_config:get(compiler_options),
  case maps:find(warnings_as_errors, CompilerOptions) of
    {ok, true} -> {noreply, Config#elixir_code_server{compilation_status=CompilationStatusNew}};
    _ -> {noreply, Config}
  end;

handle_cast({reset_warnings, CompilerPid}, Config) ->
  CompilationStatusCurrent = Config#elixir_code_server.compilation_status,
  CompilationStatusNew = maps:put(CompilerPid, ok, CompilationStatusCurrent),
  {noreply, Config#elixir_code_server{compilation_status=CompilationStatusNew}};

handle_cast({required, Path}, Config) ->
  Current = Config#elixir_code_server.required,
  case maps:find(Path, Current) of
    {ok, true} ->
      {noreply, Config};
    {ok, Queued} ->
      _ = [gen_server:reply(From, required) || From <- lists:reverse(Queued)],
      Done = maps:put(Path, true, Current),
      {noreply, Config#elixir_code_server{required=Done}};
    error ->
      Done = maps:put(Path, true, Current),
      {noreply, Config#elixir_code_server{required=Done}}
  end;

handle_cast({unrequire_files, Files}, Config) ->
  Current  = Config#elixir_code_server.required,
  Unrequired = maps:without(Files, Current),
  {noreply, Config#elixir_code_server{required=Unrequired}};

handle_cast({return_compiler_module, Module, Purgeable}, Config) ->
  {Used, Unused, Counter} = Config#elixir_code_server.mod_pool,

  ModPool =
    case Purgeable of
      true -> {Used, [Module | Unused], Counter};
      false -> {[Module | Used], Unused, Counter}
    end,

  {noreply, Config#elixir_code_server{mod_pool=ModPool}};

handle_cast(Request, Config) ->
  {stop, {badcast, Request}, Config}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, Config) ->
  {noreply, undefmodule(Ref, Config)};

handle_info(_Msg, Config) ->
  {noreply, Config}.

terminate(_Reason, _Config) ->
  ok.

code_change(_Old, Config, _Extra) ->
  {ok, Config}.

compiler_module(I) ->
  list_to_atom("elixir_compiler_" ++ integer_to_list(I)).

defmodule(Pid, Tuple, #elixir_code_server{mod_ets=ModEts} = Config) ->
  ets:insert(elixir_modules, Tuple),
  Ref = erlang:monitor(process, Pid),
  Mod = erlang:element(1, Tuple),
  {Ref, Config#elixir_code_server{mod_ets=maps:put(Ref, Mod, ModEts)}}.

undefmodule(Ref, #elixir_code_server{mod_ets=ModEts} = Config) ->
  case maps:find(Ref, ModEts) of
    {ok, Mod} ->
      ets:delete(elixir_modules, Mod),
      Config#elixir_code_server{mod_ets=maps:remove(Ref, ModEts)};
    error ->
      Config
  end.
