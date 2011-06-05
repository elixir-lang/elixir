-module(elixir_dispatch).
-export([dispatch/3,dispatch/4,super/4,builtin_mixin/1]).
-include("elixir.hrl").

dispatch(Object, Method, Args) ->
  dispatch(Object, Method, length(Args) + 1, Args).

% TODO: No need to pass arity
dispatch(#elixir_slate__{module=Module} = Object, Method, _Arity, Args) ->
  apply(Module, Method, [Object|Args]);

dispatch(#elixir_module__{name=Module,data=Data} = Object, Method, _Arity, Args) when not is_atom(Data) ->
  apply(Module, Method, [Object|Args]);

dispatch(#elixir_module__{name=Fallback} = Object, Method, Arity, Args) ->
  % Temporarily add Module::Definition so compiled method works.
  Chain = ['Module::Definition'|elixir_module_behavior:mixins(Object)],
  case find_module_chain(Chain, Method, Arity) of
    false -> Module = Fallback;
    Module -> []
  end,
  apply(Module, Method, [Object|Args]);

dispatch(Object, Method, _Arity, Args) ->
  Module = builtin_mixin(Object),
  apply(Module, Method, [Object|Args]).

super(Object, Module, Method, Args) ->
  WholeChain = elixir_module_behavior:mixins(Object),
  [Module|Chain] = lists:dropwhile(fun(X) -> X /= Module end, WholeChain),
  Arity = length(Args) + 1,
  case find_module_chain(Chain, Method, Arity) of
    false -> Next = Module;
    Next  -> []
  end,
  apply(Next, Method, [Object|Args]).

find_module_chain([], _Method, _Arity) -> false;

find_module_chain([H|T], Method, Arity) ->
  Name = ?ELIXIR_ERL_MODULE(H),
  case Name:'__elixir_exported__'(Method, Arity) of
    true -> Name;
    _ -> find_module_chain(T, Method, Arity)
  end.

% Builtin mixins

builtin_mixin(Native) when is_list(Native) ->
  'exList::Behavior';

builtin_mixin(Native) when is_binary(Native) ->
  'exString::Behavior';

builtin_mixin(Native) when is_integer(Native) ->
  'exInteger::Behavior';

builtin_mixin(Native) when is_float(Native) ->
  'exFloat::Behavior';

builtin_mixin(Native) when is_atom(Native) ->
  'exAtom::Behavior';

builtin_mixin(#elixir_orddict__{}) ->
  'exOrderedDict::Behavior';

builtin_mixin(Native) when is_bitstring(Native) ->
  'exBitString::Behavior';

builtin_mixin(Native) when is_tuple(Native) ->
  'exTuple::Behavior';

builtin_mixin(Native) when is_function(Native) ->
  'exFunction::Behavior';

builtin_mixin(Native) when is_pid(Native) ->
  'exProcess::Behavior';

builtin_mixin(Native) when is_reference(Native) ->
  'exReference::Behavior';

builtin_mixin(Native) when is_port(Native) ->
  'exPort::Behavior'.
