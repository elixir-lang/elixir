-module(elixir_dispatch).
-export([owner_dispatch/4,dispatch/3,super/4]).
-include("elixir.hrl").

owner_dispatch(Module, Self, Method, Args) ->
  Proto = (elixir_constants:lookup(Module))#elixir_object__.protos,
  apply(Proto, Method, [Self|Args]).

dispatch(Object, Method, Args) ->
  Arity = length(Args) + 1,
  case find_module(Object, Method, Arity) of
    false -> method_missing(Object, [Method, Args]);
    Module -> apply(Module, Method, [Object|Args])
  end.

super(Object, Module, Method, Args) ->
  WholeChain = elixir_object_methods:mixins(Object),
  [Module|Chain] = lists:dropwhile(fun(X) -> X /= Module end, WholeChain),
  Arity = length(Args) + 1,
  case find_module_chain(Chain, Method, Arity) of
    false -> method_missing(Object, [Method, Args]);
    Next -> apply(Next, Method, [Object|Args])
  end.

method_missing(Object, Args) ->
  case find_module(Object, method_missing, 3) of
    false -> elixir_errors:error({nomethodmissing, {Args,Object}});
    Module -> apply(Module, method_missing, [Object|Args])
  end.

% Find first module that contains the method with given arity.
find_module(#elixir_object__{mixins=Mixin}, Method, Arity) when is_atom(Mixin) ->
  case Mixin:'__function_exported__'(Method, Arity) of
    true  -> Mixin;
    false -> false
  end;

find_module(#elixir_object__{} = Object, Method, Arity) ->
  Chain = elixir_object_methods:mixins(Object),
  find_module_chain(Chain, Method, Arity);

find_module(Object, Method, Arity) ->
  Mixin = elixir_object_methods:builtin_mixin(Object),
  case erlang:function_exported(Mixin, Method, Arity) of
    true  -> Mixin;
    false -> false
  end.

find_module_chain([], _Method, _Arity) -> false;

find_module_chain([H|T], Method, Arity) ->
  Name = ?ELIXIR_ERL_MODULE(H),
  case Name:'__function_exported__'(Method, Arity) of
    true -> Name;
    _ -> find_module_chain(T, Method, Arity)
  end.
