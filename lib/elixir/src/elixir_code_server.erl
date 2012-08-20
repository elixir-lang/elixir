-module(elixir_code_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).
-record(elixir_code_server, {
  argv=[],
  loaded=[],
  at_exit=[],
  compiler_options=[{docs,true}]
}).

start_link() ->
  { ok, _ } = gen_server:start_link({local, elixir_code_server}, ?MODULE, [], []).

init(_args) ->
  process_flag(trap_exit, true),
  { ok, #elixir_code_server{} }.

handle_call({ acquire, Path }, From, Config) ->
  Current = Config#elixir_code_server.loaded,
  case orddict:find(Path, Current) of
    { ok, true } ->
      { reply, loaded, Config };
    { ok, List } when is_list(List) ->
      Queued = orddict:store(Path, [From|List], Current),
      { reply, queued, Config#elixir_code_server{loaded=Queued} };
    error ->
      Queued = orddict:store(Path, [], Current),
      { reply, proceed, Config#elixir_code_server{loaded=Queued} }
  end;

handle_call({ loaded, Path }, _From, Config) ->
  Current = Config#elixir_code_server.loaded,
  case orddict:find(Path, Current) of
    { ok, true } ->
      { reply, ok, Config };
    { ok, List } when is_list(List) ->
      [Pid ! { elixir_code_server, Path, loaded } || { Pid, _Tag } <- lists:reverse(List)],
      Done = orddict:store(Path, true, Current),
      { reply, ok, Config#elixir_code_server{loaded=Done} };
    error ->
      Done = orddict:store(Path, true, Current),
      { reply, ok, Config#elixir_code_server{loaded=Done} }
  end;

handle_call({at_exit, AtExit}, _From, Config) ->
  { reply, ok, Config#elixir_code_server{at_exit=[AtExit|Config#elixir_code_server.at_exit]} };

handle_call({argv, Argv}, _From, Config) ->
  { reply, ok, Config#elixir_code_server{argv=Argv} };

handle_call({compiler_options, Options}, _From, Config) ->
  Final = orddict:merge(fun(_,_,V) -> V end, Config#elixir_code_server.compiler_options, Options),
  { reply, ok, Config#elixir_code_server{compiler_options=Final} };

handle_call(loaded, _From, Config) ->
  { reply, [F || { F, true } <- Config#elixir_code_server.loaded], Config };

handle_call(at_exit, _From, Config) ->
  { reply, Config#elixir_code_server.at_exit, Config };

handle_call(argv, _From, Config) ->
  { reply, Config#elixir_code_server.argv, Config };

handle_call(compiler_options, _From, Config) ->
  { reply, Config#elixir_code_server.compiler_options, Config };

handle_call(_Request, _From, Config) ->
  { reply, undef, Config }.

handle_cast(_Request, Config) ->
  { noreply, Config }.

handle_info(_Request, Config) ->
  { noreply, Config }.

terminate(Reason, Config) ->
  io:format("[FATAL] ~p crashed:\n~p~n", [?MODULE, Reason]),
  io:format("[FATAL] ~p snapshot:\n~p~n", [?MODULE, Config]),
  ok.

code_change(_Old, Config, _Extra) ->
  { ok, Config }.