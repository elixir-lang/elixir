%% An Erlang module that behaves like an Elixir module
%% used for bootstraping.
-module(elixir_bootstrap).
-export(['MACRO-@'/2, '__info__'/1]).
-define(kernel, 'Elixir.Kernel').

%% Mock out @ to be a no-op unless Kernel is defined.
'MACRO-@'(Caller, Tree) ->
  case code:is_loaded(?kernel) of
    { _, _} -> ?kernel:'MACRO-@'(Caller, Tree);
    false   -> nil
  end.

'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{ '@', 1 }].
