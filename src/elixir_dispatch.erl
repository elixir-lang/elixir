-module(elixir_dispatch).
-export([dispatch/4]).
-include("elixir.hrl").

% TODO Implement method missing dispatching.
dispatch(Self, Object, Method, Args) ->
  Chain = elixir_object_methods:mixins(Object),
  Arity = length(Args) + 1,
  case find_module(Chain, Method, Arity) of
    [] ->
      Mixins = string:join(lists:map(fun atom_to_list/1, Chain), ", "),
      elixir_errors:raise(nomethod, "No visible method ~s/~w in mixins [~s]", [Method, Arity - 1, Mixins]);
    Module -> apply(Module, Method, [Object|Args])
  end.

% Find first module that contains the method with given arity.
find_module([], Method, Arity) -> [];

find_module([H|T], Method, Arity) ->
  case erlang:function_exported(H, Method, Arity) of
    true -> H;
    Else -> find_module(T, Method, Arity)
  end.