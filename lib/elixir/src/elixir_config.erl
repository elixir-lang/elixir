-module(elixir_config).
-compile({no_auto_import, [get/1]}).
-export([new/1, delete/1, put/2, get/1, get/2, update/2, get_and_put/2, warn/2, serial/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

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
  gen_server:call(?MODULE, {put, Key, Value}).

get_and_put(Key, Value) ->
  gen_server:call(?MODULE, {get_and_put, Key, Value}).

update(Key, Fun) ->
  gen_server:call(?MODULE, {update, Key, Fun}).

serial(Fun) ->
  gen_server:call(?MODULE, {serial, Fun}).

%% Used to guarantee warnings are emitted only once per caller.
warn(Key, [{Mod, Fun, ArgsOrArity, _} | _]) ->
  EtsKey = {warn, Key, Mod, Fun, to_arity(ArgsOrArity)},
  ets:update_counter(?MODULE, EtsKey, {2, 1, 1, 1}, {EtsKey, -1}) =:= 0;

warn(_, _) ->
  true.

to_arity(Args) when is_list(Args) -> length(Args);
to_arity(Arity) -> Arity.

%% ets life-cycle api

new(Opts) ->
  Tab = ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
  true = ets:insert_new(?MODULE, Opts),
  Tab.

delete(?MODULE) ->
  ets:delete(?MODULE).

%% gen_server api

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, ?MODULE, []).

init(Tab) ->
  {ok, Tab}.

handle_call({serial, Fun}, _From, Tab) ->
  {reply, Fun(), Tab};
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
