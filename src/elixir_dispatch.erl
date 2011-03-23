-module(elixir_dispatch).
-export([dispatch/3,super/4]).
-include("elixir.hrl").

dispatch(Object, Method, Args) when is_list(Args)->
  Arity = length(Args) + 1,
  case find_module(Object, Method, Arity) of
    false -> dispatch(Object, method_missing, [Method, Args]);
    Module -> apply(Module, Method, [Object|Args])
  end.

super(Object, Module, Method, Args) when is_list(Args) ->
  WholeChain = elixir_object_methods:mixins(Object),
  [Module|Chain] = lists:dropwhile(fun(X) -> X /= Module end, WholeChain),
  Arity = length(Args) + 1,
  case find_module_chain(Chain, Method, Arity) of
    false -> dispatch(Object, method_missing, [Method, Args]);
    Next -> apply(Next, Method, [Object|Args])
  end.

% % If self is true, we don't check if it is protected or not
% % and dispatch it right away.
% visibility_matches(true, _Module, _Method, _Arity) ->
%   true;
%
% % If self is false, we need to check if the method is protected,
% % if so, don't dispatch.
% visibility_matches(false, Module, Method, Arity) ->
%   not is_protected_method(Module, Method, Arity);
%
% % If self is an object, we need to check if the visibility allows
% % the method invocation.
% visibility_matches(Self, Module, Method, Arity) ->
%   case is_protected_method(Module, Method, Arity) of
%     true  -> lists:member(Module, elixir_object_methods:mixins(Self));
%     false -> true
%   end.
%
% is_protected_method(Module, Method, Arity) ->
%   Protected = elixir_methods:abstract_protected_methods(Module),
%   lists:member({Method, Arity}, Protected).

% Find first module that contains the method with given arity.
find_module(#elixir_object__{mixins=Mixin}, Method, Arity) when is_atom(Mixin) ->
  case erlang:function_exported(Mixin, Method, Arity) of
    true  -> Mixin;
    false -> false
  end;

find_module(#elixir_object__{} = Object, Method, Arity) ->
  Chain = elixir_object_methods:mixins(Object),
  find_module_chain(Chain, Method, Arity);

find_module(Object, Method, Arity) ->
  Mixin = builtin_mixin(Object),
  case erlang:function_exported(Mixin, Method, Arity) of
    true  -> Mixin;
    false -> false
  end.

find_module_chain([], _Method, _Arity) -> false;

find_module_chain([H|T], Method, Arity) ->
  case erlang:function_exported(H, Method, Arity) of
    true -> H;
    _ -> find_module_chain(T, Method, Arity)
  end.

builtin_mixin(Native) when is_integer(Native) ->
  'Integer::Proto';

builtin_mixin(Native) when is_float(Native) ->
  'Float::Proto';

builtin_mixin(Native) when is_atom(Native) ->
  'Atom::Proto';

builtin_mixin(Native) when is_list(Native) ->
  'List::Proto';

builtin_mixin(Native) when is_bitstring(Native) ->
  'BitString::Proto';

builtin_mixin(#elixir_orddict__{}) ->
  'OrderedDict::Proto';

builtin_mixin(#elixir_string__{}) ->
  'String::Proto';

builtin_mixin(Native) when is_tuple(Native) ->
  'Tuple::Proto';

builtin_mixin(Native) when is_function(Native) ->
  'Function::Proto';

builtin_mixin(Native) when is_pid(Native) ->
  'Process::Proto';

builtin_mixin(Native) when is_reference(Native) ->
  'Reference::Proto';

builtin_mixin(Native) when is_port(Native) ->
  'Port::Proto'.