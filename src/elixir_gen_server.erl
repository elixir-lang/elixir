-module(elixir_gen_server).
-behavior(gen_server).

init(State) -> elixir_dispatch:dispatch(true, State, init, []).
handle_call(Req, From, State) -> elixir_dispatch:dispatch(true, State, handle_call, [Req, From]).
handle_cast(Req, State) -> elixir_dispatch:dispatch(true, State, handle_cast, [Req]).
handle_info(Req, State) -> elixir_dispatch:dispatch(true, State, handle_info, [Req]).
terminate(Req, State) -> elixir_dispatch:dispatch(true, State, terminate, [Req]).
code_change(Old, State, Extra) -> elixir_dispatch:dispatch(true, State, code_change, [Old, Extra]).