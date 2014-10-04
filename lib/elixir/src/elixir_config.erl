-module(elixir_config).
-compile({no_auto_import, [get/1]}).
-export([new/1, delete/1, put/2, get/1, update/2, get_and_put/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
  handle_info/2, code_change/3, terminate/2]).
-behaviour(gen_server).

%% public api

new(Opts) ->
  Tab = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
  true = ets:insert_new(?MODULE, Opts),
  Tab.

delete(?MODULE) ->
  ets:delete(?MODULE).

put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
  case ets:lookup(?MODULE, Key) of
    [{_, Value}] -> Value;
    []          -> nil
  end.

update(Key, Fun) ->
  gen_server:call(?MODULE, {update, Key, Fun}).

get_and_put(Key, Value) ->
  gen_server:call(?MODULE, {get_and_put, Key, Value}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

%% gen_server api

init(Tab) ->
  %% Ets table must be writable
  public = ets:info(Tab, protection),
  {ok, Tab}.

handle_call({put, Key, Value}, _From, Tab) ->
  ets:insert(Tab, {Key, Value}),
  {reply, ok, Tab};
handle_call({update, Key, Fun}, _From, Tab) ->
  Value = Fun(get(Key)),
  ets:insert(Tab, {Key, Value}),
  {reply, Value, Tab};
handle_call({get_and_put, Key, Value}, _From, Tab) ->
  OldValue = get(Key),
  ets:insert(Tab, {Key, Value}),
  {reply, OldValue, Tab}.

handle_cast(Cast, Tab) ->
  {stop, {bad_cast, Cast}, Tab}.

handle_info(_Msg, Tab) ->
  {noreply, Tab}.

code_change(_OldVsn, Tab, _Extra) ->
  {ok, Tab}.

terminate(_Reason, _Tab) ->
  ok.
