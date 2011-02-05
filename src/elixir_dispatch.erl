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
      Message = "No method ~s/~w in mixins [~s]",
      elixir_errors:raise(nomethod, Message, [Method, Arity - 1, Mixins]);
    Module ->
      case visibility_matches(Self, Module, Method, Arity) of
        true  -> apply(Module, Method, [Object|Args]);
        false ->
          Message = "Cannot invoke protected method ~s/~w in mixin ~s",
          elixir_errors:raise(protectedmethod, Message, [Method, Arity - 1, atom_to_list(Module)])
      end
  end.

visibility_matches([], Module, Method, Arity) ->
  true;

visibility_matches(Self, Module, Method, Arity) ->
  Protected = proplists:get_value(protected, elixir_constants:lookup_attributes(Module)),
  case lists:member({Method, Arity}, Protected) of
    true  -> lists:member(Module, elixir_object_methods:mixins(Self));
    false -> true
  end.

% Find first module that contains the method with given arity.
find_module([], Method, Arity) -> [];

find_module([H|T], Method, Arity) ->
  case erlang:function_exported(H, Method, Arity) of
    true -> H;
    Else -> find_module(T, Method, Arity)
  end.