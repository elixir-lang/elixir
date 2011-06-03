-module(elixir_bind).
-export([slate_bind/2, bind/3]).
-include("elixir.hrl").

slate_bind(Right, Args) ->
  check_module(Right, bind),
  Bound = #elixir_slate__{module=Right},
  apply(Right, '__bound__', [Bound|Args]).

bind(#elixir_slate__{module=[]} = Self, Right, Args) ->
  check_module(Right, bind),
  Bound = Self#elixir_slate__{module=Right},
  apply(Right, '__bound__', [Bound|Args]);

bind(#elixir_slate__{} = Self, Right, Args) ->
  elixir_errors:error({already_bound, {Self,Right,Args}});

bind(Self, Right, Args) ->
  elixir_errors:error({binding_not_allowed, {Self,Right,Args}}).

% Helpers

check_module(#elixir_object__{parent='Module'}, Kind) -> [];
check_module(Else, Kind) -> elixir_errors:error({not_a_module, Else}).