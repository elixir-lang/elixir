-module(elixir_dispatch).
-export([dispatch/3]).
-include("elixir.hrl").

dispatch(#elixir_object{} = Object, Method, Args) ->
  Module = Object#elixir_object.name,
  apply(Module, Method, [Object|Args]);

dispatch(Else, Method, Args) ->
  Format = io_lib:format("~p"),
  erlang:error("Unknown type " ++ Format ++ " to dispatch method " ++ atom_to_list(Method)).