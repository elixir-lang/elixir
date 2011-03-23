-module(elixir_gen_server).
-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(State) ->
  elixir_methods:assert_behavior(?MODULE, State),
  elixir_dispatch:dispatch(State, init, []).

handle_call(Req, From, State) -> elixir_dispatch:dispatch(State, handle_call, [Req, From]).
handle_cast(Req, State) -> elixir_dispatch:dispatch(State, handle_cast, [Req]).
handle_info(Req, State) -> elixir_dispatch:dispatch(State, handle_info, [Req]).
terminate(Req, State) -> elixir_dispatch:dispatch(State, terminate, [Req]).
code_change(Old, State, Extra) -> elixir_dispatch:dispatch(State, code_change, [Old, Extra]).