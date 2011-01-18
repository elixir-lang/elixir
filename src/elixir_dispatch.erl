-module(elixir_dispatch).
-export([dispatch/3]).
-include("elixir.hrl").

% TODO Implement method missing dispatching.
dispatch(Object, Method, Args) ->
  Chain = dispatch_chain(Object),
  Arity = length(Args) + 1,
  case find_module(Chain, Method, Arity) of
    []     -> ?ELIXIR_ERROR(nomethod, "No method ~p/~p in mixins ~p", [Method, Arity - 1, Chain]);
    Module -> apply(Module, Method, [Object|Args])
  end;

dispatch(Else, Method, Args) ->
  ?ELIXIR_ERROR(nomethod, "Unknown type ~p to dispatch method ~p", [Else, Method]).

% Get mixins allowing erlang types like Integer.
dispatch_chain(#elixir_object{} = Object) ->
  elixir_object_methods:dispatch_chain(Object);

dispatch_chain(Object) when is_integer(Object) ->
  elixir_object_methods:dispatch_chain('Integer');

dispatch_chain(Object) when is_float(Object) ->
  elixir_object_methods:dispatch_chain('Float').

% Find first module that contains the method with given arity.
find_module([H|T], Method, Arity) ->
  case erlang:function_exported(H, Method, Arity) of
    true -> H;
    Else -> find_module(T, Method, Arity)
  end;

find_module([], Method, Arity) -> [].