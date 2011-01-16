-module(elixir_dispatch).
-export([dispatch/3]).
-include("elixir.hrl").

% TODO Implement method missing dispatching.
dispatch(Object, Method, Args) ->
  Mixins = get_mixins(Object),
  Arity  = length(Args) + 1,
  case find_module(Mixins, Method, Arity) of
    []     -> ?ELIXIR_ERROR(nomethod, "No method ~p/~p in mixins ~p", [Method, Arity - 1, Mixins]);
    Module -> apply(Module, Method, [Object|Args])
  end;

dispatch(Else, Method, Args) ->
  ?ELIXIR_ERROR(nomethod, "Unknown type ~p to dispatch method ~p", [Else, Method]).

% Get mixins allowing erlang types like Integer.
get_mixins(#elixir_object{mixins=Mixins}) ->
  Mixins;

get_mixins(Object) when is_integer(Object) ->
  get_protos('Integer');

get_mixins(Object) when is_float(Object) ->
  get_protos('Float').

% If we have an Erlang type, get the object definition and protos.
get_protos(Name) ->
  proplists:get_value(protos, Name:module_info(attributes)).

% Find first module that contains the method with given arity.
find_module([H|T], Method, Arity) ->
  case erlang:function_exported(H, Method, Arity) of
    true -> H;
    Else -> find_module(T, Method, Arity)
  end;

find_module([], Method, Arity) -> [].