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

'MACRO-def'(Caller, Call) -> 'MACRO-def'(Caller, Call, nil).
'MACRO-def'(Caller, Call, Expr) -> define(Caller, def, Call, Expr).
'MACRO-defp'(Caller, Call, Expr) -> define(Caller, defp, Call, Expr).

'MACRO-defmacro'(Caller, Call) -> 'MACRO-defmacro'(Caller, Call, nil).
'MACRO-defmacro'(Caller, Call, Expr) -> define(Caller, defmacro, Call, Expr).
'MACRO-defmacrop'(Caller, Call, Expr) -> define(Caller, defmacrop, Call, Expr).

'MACRO-defmodule'(_Caller, Alias, [{do, Block}]) ->
  Escaped = elixir_quote:escape(Block, none, false),
  Args = [Alias, Escaped, [], false, env()],
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

define({Line, _S, #{module := Module} = E}, Kind, Call, Expr) ->
  UC = elixir_quote:has_unquotes(Call),
  UE = elixir_quote:has_unquotes(Expr),

  Store =
    case UC or UE of
      true ->
        elixir_quote:escape({Call, Expr}, none, true);

      false ->
        Key = erlang:unique_integer(),
        elixir_module:write_cache(Module, Key, {Call, Expr}),
        Key
    end,

  Args = [Kind, Store, elixir_locals:cache_env(E#{line := Line})],
  {{'.', [], [elixir_def, store_definition]}, [], Args}.

unless_loaded(Fun, Args, Callback) ->
  case erlang:module_loaded(?kernel) of
    true -> apply(?kernel, Fun, Args);
    false -> Callback()
  end.

env() ->
  {'__ENV__', [], nil}.
