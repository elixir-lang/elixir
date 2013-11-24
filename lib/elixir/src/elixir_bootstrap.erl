%% An Erlang module that behaves like an Elixir module
%% used for bootstraping.
-module(elixir_bootstrap).
-export(['MACRO-def'/2, 'MACRO-def'/3, 'MACRO-defp'/3, 'MACRO-defmodule'/3,
         'MACRO-defmacro'/2, 'MACRO-defmacro'/3, 'MACRO-defmacrop'/3,
         'MACRO-@'/2, '__info__'/1]).
-define(kernel, 'Elixir.Kernel').

%% Mock out @ to be a no-op unless Kernel is defined.
'MACRO-@'(Caller, Tree) ->
  unless_loaded('MACRO-@', [Caller, Tree], fun
    () -> nil
  end).

'MACRO-def'(Caller, Call) -> 'MACRO-def'(Caller, Call, nil).
'MACRO-def'(Caller, Call, Expr) -> definition(Caller, def, Call, Expr).
'MACRO-defp'(Caller, Call, Expr) -> definition(Caller, defp, Call, Expr).

'MACRO-defmacro'(Caller, Call) -> 'MACRO-defmacro'(Caller, Call, nil).
'MACRO-defmacro'(Caller, Call, Expr) -> definition(Caller, defmacro, Call, Expr).
'MACRO-defmacrop'(Caller, Call, Expr) -> definition(Caller, defmacrop, Call, Expr).

'MACRO-defmodule'(Caller, Alias, [{do,Block}]) ->
  ExEnv = elixir_env:scope_to_ex(Caller),
  Env = elixir_env:ex_to_env(ExEnv),

  { Escaped, _ } = elixir_quote:escape(Block, false),
  QuotedEnv = elixir_env:serialize(Env),

  Args = [Alias, Escaped, [], QuotedEnv],
  { { '.', [], [elixir_module, compile] }, [], Args }.

'__info__'(functions) ->
  [];
'__info__'(macros) ->
  [{ '@', 1 },
   {def,1},
   {def,2},
   {defmacro,1},
   {defmacro,2},
   {defmacrop,1},
   {defmacrop,2},
   {defmodule,2},
   {defp,1},
   {defp,2}].

definition(Caller, Kind, Call, Expr) ->
  Env = elixir_env:scope_to_ex(Caller),
  elixir_def:wrap_definition(Kind, Call, Expr, Env).

unless_loaded(Fun, Args, Callback) ->
  case code:is_loaded(?kernel) of
    { _, _} -> apply(?kernel, Fun, Args);
    false   -> Callback()
  end.
