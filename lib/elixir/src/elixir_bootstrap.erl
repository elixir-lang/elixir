%% An Erlang module that behaves like an Elixir module
%% used for bootstrapping.
-module(elixir_bootstrap).
-export(['MACRO-def'/2, 'MACRO-def'/3, 'MACRO-defp'/3, 'MACRO-defmodule'/3,
         'MACRO-defmacro'/2, 'MACRO-defmacro'/3, 'MACRO-defmacrop'/3,
         'MACRO-@'/2, '__info__'/1]).
-define(kernel, 'Elixir.Kernel').

%% Mock out @ to be a no-op unless Kernel is defined.
'MACRO-@'(Caller, Tree) ->
  unless_loaded('MACRO-@', [Caller, Tree], fun() -> nil end).

'MACRO-def'(Caller, Call) -> 'MACRO-def'(Caller, Call, []).
'MACRO-def'(Caller, Call, Expr) -> define(Caller, def, Call, Expr).
'MACRO-defp'(Caller, Call, Expr) -> define(Caller, defp, Call, Expr).

'MACRO-defmacro'(Caller, Call) -> 'MACRO-defmacro'(Caller, Call, []).
'MACRO-defmacro'(Caller, Call, Expr) -> define(Caller, defmacro, Call, Expr).
'MACRO-defmacrop'(Caller, Call, Expr) -> define(Caller, defmacrop, Call, Expr).

'MACRO-defmodule'(_Caller, Alias, [{do, Block}]) ->
  Escaped = elixir_quote:escape(Block, default, false),
  Args = [Alias, Escaped, [], env()],
  {{'.', [], [elixir_module, compile]}, [], Args}.

'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{'@', 1},
   {def, 1},
   {def, 2},
   {defmacro, 1},
   {defmacro, 2},
   {defmacrop, 2},
   {defmodule, 2},
   {defp, 2}].

define({Line, E}, Kind, Call, Expr) ->
  UC = elixir_quote:has_unquotes(Call),
  UE = elixir_quote:has_unquotes(Expr),
  EscapedCall = elixir_quote:escape(Call, default, true),
  EscapedExpr = elixir_quote:escape(Expr, default, true),
  Args = [Kind, not(UC or UE), EscapedCall, EscapedExpr, elixir_locals:cache_env(E#{line := Line})],
  {{'.', [], [elixir_def, store_definition]}, [], Args}.

unless_loaded(Fun, Args, Callback) ->
  case code:is_loaded(?kernel) of
    {_, _} -> apply(?kernel, Fun, Args);
    false  -> Callback()
  end.

env() ->
  {'__ENV__', [], nil}.
