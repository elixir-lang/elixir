-module(elixir_error_handler).
-export([undefined_function/3, undefined_lambda/3]).

undefined_function(Module, Func, Args) ->
  error_handler:undefined_function(Module, Func, Args).

undefined_lambda(Module, Func, Args) ->
  error_handler:undefined_lambda(Module, Func, Args).