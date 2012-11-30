-module(elixir_code_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).
-record(elixir_code_server, {
  argv=[],
  loaded=[],
  at_exit=[],
  pool=[],
  counter=0,
  compiler_options=[{docs,true},{debug_info,true}],
  waiting=[]
}).

start_link() ->
  { ok, _ } = gen_server:start_link({local, elixir_code_server}, ?MODULE, [], []).

init(_args) ->
  { ok, #elixir_code_server{} }.

handle_call({ wait_until_finished, Pid }, _, Config) ->
  Waiting = Config#elixir_code_server.waiting,
  case is_list(Waiting) of
    true  -> { reply, wait, Config#elixir_code_server{waiting=[Pid|Waiting]} };
    false -> { reply, ok, Config }
  end;

handle_call({ acquire, Path }, From, Config) ->
  Current = Config#elixir_code_server.loaded,
  case orddict:find(Path, Current) of
    { ok, true } ->
      { reply, loaded, Config };
    { ok, { Ref, List } } when is_list(List), is_reference(Ref) ->
      Queued = orddict:store(Path, { Ref, [From|List] }, Current),
      { reply, { queued, Ref }, Config#elixir_code_server{loaded=Queued} };
    error ->
      Queued = orddict:store(Path, { make_ref(), [] }, Current),
      { reply, proceed, Config#elixir_code_server{loaded=Queued} }
  end;

handle_call({ at_exit, AtExit }, _From, Config) ->
  { reply, ok, Config#elixir_code_server{at_exit=[AtExit|Config#elixir_code_server.at_exit]} };

handle_call({ argv, Argv }, _From, Config) ->
  { reply, ok, Config#elixir_code_server{argv=Argv} };

handle_call({ compiler_options, Options }, _From, Config) ->
  Final = orddict:merge(fun(_,_,V) -> V end, Config#elixir_code_server.compiler_options, Options),
  { reply, ok, Config#elixir_code_server{compiler_options=Final} };

handle_call(loaded, _From, Config) ->
  { reply, [F || { F, true } <- Config#elixir_code_server.loaded], Config };

handle_call(at_exit, _From, Config) ->
  { reply, Config#elixir_code_server.at_exit, Config };

handle_call(flush_at_exit, _From, Config) ->
  { reply, Config#elixir_code_server.at_exit, Config#elixir_code_server{at_exit=[]} };

handle_call(argv, _From, Config) ->
  { reply, Config#elixir_code_server.argv, Config };

handle_call(compiler_options, _From, Config) ->
  { reply, Config#elixir_code_server.compiler_options, Config };

handle_call(retrieve_module_name, _From, Config) ->
  case Config#elixir_code_server.pool of
    [H|T] ->
      { reply, module_tuple(H), Config#elixir_code_server{pool=T} };
    [] ->
      Counter = Config#elixir_code_server.counter,
      { reply, module_tuple(Counter), Config#elixir_code_server{counter=Counter+1} }
  end;

handle_call(_Request, _From, Config) ->
  { reply, undef, Config }.

handle_cast(finished, Config) ->
  Waiting = Config#elixir_code_server.waiting,
  [Pid ! { elixir_code_server, finished } || Pid <- lists:reverse(Waiting)],
  { noreply, Config#elixir_code_server{waiting=done} };

handle_cast({ loaded, Path }, Config) ->
  Current = Config#elixir_code_server.loaded,
  case orddict:find(Path, Current) of
    { ok, true } ->
      { noreply, Config };
    { ok, { Ref, List } } when is_list(List), is_reference(Ref) ->
      [Pid ! { elixir_code_server, Ref, loaded } || { Pid, _Tag } <- lists:reverse(List)],
      Done = orddict:store(Path, true, Current),
      { noreply, Config#elixir_code_server{loaded=Done} };
    error ->
      Done = orddict:store(Path, true, Current),
      { noreply, Config#elixir_code_server{loaded=Done} }
  end;

handle_cast({ unload_files, Files }, Config) ->
  Current  = Config#elixir_code_server.loaded,
  Unloaded = lists:foldl(fun(File, Acc) -> orddict:erase(File, Acc) end, Current, Files),
  { noreply, Config#elixir_code_server{loaded=Unloaded} };

handle_cast({ return_module_name, I }, #elixir_code_server{pool=Pool} = Config) ->
  { noreply, Config#elixir_code_server{pool=[I|Pool]} };

handle_cast(_Request, Config) ->
  { noreply, Config }.

handle_info(_Request, Config) ->
  { noreply, Config }.

terminate(_Reason, _Config) ->
  ok.

code_change(_Old, Config, _Extra) ->
  { ok, Config }.

module_tuple(I) -> { list_to_atom("elixir_compiler_" ++ integer_to_list(I)), I }.