-module(elixir_code_server).
-export([call/1, cast/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).
-behaviour(gen_server).

-define(timeout, 30000).
-record(elixir_code_server, {
  argv=[],
  loaded=[],
  at_exit=[],
  paths={[],[]},
  mod_pool={[],0},
  mod_ets=dict:new(),
  compilation_status=[],
  compiler_options=[{docs,true},{debug_info,true},{warnings_as_errors,false}],
  erl_compiler_options=nil
}).

call(Args) ->
  gen_server:call(?MODULE, Args, ?timeout).

cast(Args) ->
  gen_server:cast(?MODULE, Args).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).

init(ok) ->
  %% We attempt to load those modules here so throughout
  %% the codebase we can use a code:is_loaded/1 check on
  %% bootstrap instead of using the more expensive
  %% code:ensure_loaded/1 multiple times.
  _ = code:ensure_loaded('Elixir.Macro.Env'),
  _ = code:ensure_loaded('Elixir.Module.LocalsTracker'),
  _ = code:ensure_loaded('Elixir.Kernel.LexicalTracker'),

  %% The table where we store module definitions
  _ = ets:new(elixir_modules, [set, protected, named_table, {read_concurrency, true}]),

  {ok, #elixir_code_server{}}.

handle_call({defmodule, Pid, Tuple}, _From, Config) ->
  {Ref, New} = defmodule(Pid, Tuple, Config),
  {reply, Ref, New};

handle_call({undefmodule, Ref}, _From, Config) ->
  {reply, ok, undefmodule(Ref, Config)};

handle_call({acquire, Path}, From, Config) ->
  Current = Config#elixir_code_server.loaded,
  case orddict:find(Path, Current) of
    {ok, true} ->
      {reply, loaded, Config};
    {ok, {Ref, List}} when is_list(List), is_reference(Ref) ->
      Queued = orddict:store(Path, {Ref, [From|List]}, Current),
      {reply, {queued, Ref}, Config#elixir_code_server{loaded=Queued}};
    error ->
      Queued = orddict:store(Path, {make_ref(), []}, Current),
      {reply, proceed, Config#elixir_code_server{loaded=Queued}}
  end;

handle_call(loaded, _From, Config) ->
  {reply, [F || {F, true} <- Config#elixir_code_server.loaded], Config};

handle_call(at_exit, _From, Config) ->
  {reply, Config#elixir_code_server.at_exit, Config};

handle_call(flush_at_exit, _From, Config) ->
  {reply, Config#elixir_code_server.at_exit, Config#elixir_code_server{at_exit=[]}};

handle_call(argv, _From, Config) ->
  {reply, Config#elixir_code_server.argv, Config};

handle_call(compiler_options, _From, Config) ->
  {reply, Config#elixir_code_server.compiler_options, Config};

handle_call({compilation_status, CompilerPid}, _From, Config) ->
  CompilationStatusList    = Config#elixir_code_server.compilation_status,
  CompilationStatusListNew = orddict:erase(CompilerPid, CompilationStatusList),
  CompilationStatus        = orddict:fetch(CompilerPid, CompilationStatusList),
  {reply, CompilationStatus,
   Config#elixir_code_server{compilation_status=CompilationStatusListNew}};

handle_call(retrieve_module_name, _From, Config) ->
  case Config#elixir_code_server.mod_pool of
    {[H|T], Counter} ->
      {reply, module_tuple(H), Config#elixir_code_server{mod_pool={T,Counter}}};
    {[], Counter} ->
      {reply, module_tuple(Counter), Config#elixir_code_server{mod_pool={[],Counter+1}}}
  end;

handle_call(erl_compiler_options, _From, Config) ->
  case Config#elixir_code_server.erl_compiler_options of
    nil ->
      Opts = erl_compiler_options(),
      {reply, Opts, Config#elixir_code_server{erl_compiler_options=Opts}};
    Opts ->
      {reply, Opts, Config}
  end;

handle_call(paths, _From, Config) ->
  {reply, Config#elixir_code_server.paths, Config};

handle_call(Request, _From, Config) ->
  {stop, {badcall, Request}, Config}.

handle_cast({at_exit, AtExit}, Config) ->
  {noreply, Config#elixir_code_server{at_exit=[AtExit|Config#elixir_code_server.at_exit]}};

handle_cast({argv, Argv}, Config) ->
  {noreply, Config#elixir_code_server{argv=Argv}};

handle_cast({compiler_options, Options}, Config) ->
  Final = orddict:merge(fun(_,_,V) -> V end, Config#elixir_code_server.compiler_options, Options),
  {noreply, Config#elixir_code_server{compiler_options=Final}};

handle_cast({register_warning, CompilerPid}, Config) ->
  CompilationStatusCurrent = Config#elixir_code_server.compilation_status,
  CompilationStatusNew     = orddict:store(CompilerPid, error, CompilationStatusCurrent),
  case orddict:find(warnings_as_errors, Config#elixir_code_server.compiler_options) of
    {ok, true} -> {noreply, Config#elixir_code_server{compilation_status=CompilationStatusNew}};
    _ -> {noreply, Config}
  end;

handle_cast({reset_warnings, CompilerPid}, Config) ->
  CompilationStatusCurrent = Config#elixir_code_server.compilation_status,
  CompilationStatusNew     = orddict:store(CompilerPid, ok, CompilationStatusCurrent),
  {noreply, Config#elixir_code_server{compilation_status=CompilationStatusNew}};

handle_cast({loaded, Path}, Config) ->
  Current = Config#elixir_code_server.loaded,
  case orddict:find(Path, Current) of
    {ok, true} ->
      {noreply, Config};
    {ok, {Ref, List}} when is_list(List), is_reference(Ref) ->
      _ = [Pid ! {elixir_code_server, Ref, loaded} || {Pid, _Tag} <- lists:reverse(List)],
      Done = orddict:store(Path, true, Current),
      {noreply, Config#elixir_code_server{loaded=Done}};
    error ->
      Done = orddict:store(Path, true, Current),
      {noreply, Config#elixir_code_server{loaded=Done}}
  end;

handle_cast({unload_files, Files}, Config) ->
  Current  = Config#elixir_code_server.loaded,
  Unloaded = lists:foldl(fun(File, Acc) -> orddict:erase(File, Acc) end, Current, Files),
  {noreply, Config#elixir_code_server{loaded=Unloaded}};

handle_cast({return_module_name, H}, #elixir_code_server{mod_pool={T,Counter}} = Config) ->
  {noreply, Config#elixir_code_server{mod_pool={[H|T],Counter}}};

handle_cast({paths, PA, PZ}, #elixir_code_server{} = Config) ->
  {noreply, Config#elixir_code_server{paths={PA,PZ}}};

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

module_tuple(I) ->
  {list_to_atom("elixir_compiler_" ++ integer_to_list(I)), I}.

defmodule(Pid, Tuple, #elixir_code_server{mod_ets=ModEts} = Config) ->
  ets:insert(elixir_modules, Tuple),
  Ref = erlang:monitor(process, Pid),
  Mod = erlang:element(1, Tuple),
  {Ref, Config#elixir_code_server{mod_ets=dict:store(Ref, Mod, ModEts)}}.

undefmodule(Ref, #elixir_code_server{mod_ets=ModEts} = Config) ->
  case dict:find(Ref, ModEts) of
    {ok, Mod} ->
      ets:delete(elixir_modules, Mod),
      Config#elixir_code_server{mod_ets=dict:erase(Ref, ModEts)};
    error ->
      Config
  end.

erl_compiler_options() ->
  Key = "ERL_COMPILER_OPTIONS",
  case os:getenv(Key) of
    false -> [];
    Str when is_list(Str) ->
      case erl_scan:string(Str) of
        {ok,Tokens,_} ->
          case erl_parse:parse_term(Tokens ++ [{dot, 1}]) of
            {ok,List} when is_list(List) -> List;
            {ok,Term} -> [Term];
            {error,_Reason} ->
              io:format("Ignoring bad term in ~ts\n", [Key]),
              []
          end;
        {error, {_,_,_Reason}, _} ->
          io:format("Ignoring bad term in ~ts\n", [Key]),
          []
      end
  end.
