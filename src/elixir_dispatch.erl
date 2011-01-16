-module(elixir_dispatch).
-export([dispatch/3]).
-include("elixir.hrl").

% TODO Implement method missing dispatching.
dispatch(#elixir_object{} = Object, Method, Args) ->
  Mixins = Object#elixir_object.mixins,
  Arity  = length(Args) + 1,
  case find_module(Mixins, Method, Arity) of
    []     -> ?ELIXIR_ERROR(nomethod, "No method ~p in mixins ~p", [Method, Mixins]);
    Module -> apply(Module, Method, [Object|Args])
  end;

dispatch(Else, Method, Args) ->
  ?ELIXIR_ERROR(nomethod, "Unknown type ~p to dispatch method ~p", [Else, Method]).

% Find first module that F() returns true and returns it.
find_module([H|T], Method, Arity) ->
  case erlang:function_exported(H, Method, Arity) of
    true -> H;
    Else -> find_module(T, Method, Arity)
  end;

find_module([], Method, Arity) -> [].