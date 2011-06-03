-module(elixir_bind).
-export([slate_bind/2, bind/3]).
-include("elixir.hrl").

% TODO: assert_same

slate_bind(Right, Args) ->
  check_module(Right),
  Module = Right#elixir_object__.name,
  Bound = #elixir_slate__{module=Module},
  apply(Module, '__bound__', [Bound|Args]).

bind(#elixir_slate__{module=[]} = Left, Right, Args) ->
  check_module(Right),
  Module = Right#elixir_object__.name,
  Bound = Left#elixir_slate__{module=Module},
  apply(Module, '__bound__', [Bound|Args]);

bind(#elixir_slate__{} = Self, Right, Args) ->
  elixir_errors:error({already_bound, {Self,Right,Args}});

bind(Self, Right, Args) ->
  elixir_errors:error({binding_not_allowed, {Self,Right,Args}}).

% Helpers

check_module(#elixir_object__{}) -> [];
check_module(Else) -> elixir_errors:error({not_a_module, Else}).