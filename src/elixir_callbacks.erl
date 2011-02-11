% Defines a callback module given the module Name, Behavior and Callback methods.
-module(elixir_callbacks).
-export([behaviour_info/1, build_module_form/4, callback_name/1]).
-include("elixir.hrl").

behaviour_info(callbacks) -> [];
behaviour_info(_) -> undefined.

callback_name(Name) ->
  ?ELIXIR_ATOM_CONCAT(["ex_callbacks_", Name]).

build_module_form(Line, Name, Behavior, Callbacks) ->
  TransFuns   = fun(X) -> build_function_form(Line, Name, X) end,
  TransExport = fun({Fun, Arity}) -> {Fun, Arity - 1} end,
  Functions   = lists:map(TransFuns, Callbacks),
  Export      = lists:map(TransExport, Callbacks),

  [{attribute, Line, module, callback_name(Name)},
   {attribute, Line, behavior, Behavior}, {attribute, Line, export, Export} | Functions].

build_function_form(Line, Module, {Name, ElixirArity}) ->
  Arity = ElixirArity - 1,
  Args  = build_args(Line, Arity, []),
  Const = ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Module}]),
  Call  = ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [
    {atom, Line, false}, Const, {atom, Line, Name}, build_list(Line, Args)
  ]),
  {function, Line, Name, Arity,
    [{clause, Line, Args, [], [Call]}]
  }.

build_args(_Line, 0, Acc)    -> Acc;
build_args(Line, Arity, Acc) ->
  Name = ?ELIXIR_ATOM_CONCAT(["X", Arity]),
  Var  = { variable, Line, Name },
  build_args(Line, Arity - 1, [Var|Acc]).

build_list(Line, Args)       -> build_list(Line, lists:reverse(Args), {nil, Line}).
build_list(_Line, [], Acc)   -> Acc;
build_list(Line, [H|T], Acc) -> { cons, Line, H, Acc }.