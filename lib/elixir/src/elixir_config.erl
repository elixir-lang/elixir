-module(elixir_config).
-compile({no_auto_import, [get/1]}).
-export([new/1, warn/2, serial/1, booted/0, wait_until_booted/0]).
-export([static/1, is_bootstrap/0, identifier_tokenizer/0]).
-export([delete/1, put/2, get/1, get/2, update/2, get_and_put/2]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

%% Persistent term

static(Map) when is_map(Map) ->
  persistent_term:put(?MODULE, maps:merge(persistent_term:get(?MODULE, #{}), Map)).
is_bootstrap() ->
  maps:get(bootstrap, persistent_term:get(?MODULE, #{}), false).
identifier_tokenizer() ->
  maps:get(identifier_tokenizer, persistent_term:get(?MODULE, #{}), 'Elixir.String.Tokenizer').

%% Key-value store (concurrent reads, serial writes)

get(Key) ->
  [{_, Value}] = ets:lookup(?MODULE, Key),
  Value.

get(Key, Default) ->
  try ets:lookup(?MODULE, Key) of
    [{_, Value}] -> Value;
    [] -> Default
  catch
    _:_ -> Default
  end.

put(Key, Value) ->
  gen_server:call(?MODULE, {put, Key, Value}, infinity).

get_and_put(Key, Value) ->
  gen_server:call(?MODULE, {get_and_put, Key, Value}, infinity).

update(Key, Fun) ->
  gen_server:call(?MODULE, {update, Key, Fun}, infinity).

new(Opts) ->
  Tab = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
  true = ets:insert_new(?MODULE, Opts),
  Tab.

delete(?MODULE) ->
  ets:delete(?MODULE).

%% MISC

booted() ->
  gen_server:call(?MODULE, booted, infinity).
wait_until_booted() ->
  gen_server:call(?MODULE, wait_until_booted, infinity).

serial(Fun) ->
  gen_server:call(?MODULE, {serial, Fun}, infinity).

%% Used to guarantee warnings are emitted only once per caller.
warn(Key, [{Mod, Fun, ArgsOrArity, _} | _]) ->
  EtsKey = {warn, Key, Mod, Fun, to_arity(ArgsOrArity)},
  ets:update_counter(?MODULE, EtsKey, {2, 1, 1, 1}, {EtsKey, -1}) =:= 0;

warn(_, _) ->
  true.

to_arity(Args) when is_list(Args) -> length(Args);
to_arity(Arity) -> Arity.

%% gen_server api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

init(?MODULE) ->
  {ok, []}.

handle_call(booted, _From, Booted) when is_list(Booted) ->
  [gen_server:reply(Caller, ok) || Caller <- Booted],
  {reply, ok, done};
handle_call(wait_until_booted, From, Booted) ->
  if
    is_list(Booted) -> {noreply, [From | Booted]};
    Booted =:= done -> {reply, ok, Booted}
  end;
handle_call({serial, Fun}, _From, Booted) ->
  {reply, Fun(), Booted};
handle_call({put, Key, Value}, _From, Booted) ->
  ets:insert(?MODULE, {Key, Value}),
  {reply, ok, Booted};
handle_call({update, Key, Fun}, _From, Booted) ->
  Value = Fun(get(Key)),
  ets:insert(?MODULE, {Key, Value}),
  {reply, Value, Booted};
handle_call({get_and_put, Key, Value}, _From, Booted) ->
  OldValue = get(Key),
  ets:insert(?MODULE, {Key, Value}),
  {reply, OldValue, Booted}.

handle_cast(Cast, Tab) ->
  {stop, {bad_cast, Cast}, Tab}.
