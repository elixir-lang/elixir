% Defines a callback module given the module Name, Behavior and Callback methods.
-module(elixir_callbacks).
-export([behaviour_info/1, build_module_form/4, callback_name/1, behavior/1]).
-include("elixir.hrl").

behaviour_info(callbacks) -> [];
behaviour_info(_) -> undefined.

callback_name(#elixir_object__{parent='Module',name=Name}) ->
  ?ELIXIR_ATOM_CONCAT(["ex_callbacks_", Name]);

callback_name(#elixir_object__{name=Name}) ->
  ?ELIXIR_ATOM_CONCAT(["ex_callbacks_", Name, "::Proto"]).

behavior(#elixir_object__{data=Data, parent='Module'}) when is_atom(Data) ->
  case ets:lookup(Data, behavior) of
    [{behavior,Behavior}] -> Behavior;
    _ -> []
  end;

behavior(#elixir_object__{data=Data}) when is_atom(Data) ->
  case ets:lookup(Data, module) of
    [{module,Attributes}] ->
      case proplists:get_value(behavior, Attributes) of
        undefined -> [];
        Else -> Else
      end;
    _ -> []
  end;

behavior(#elixir_object__{name=Name} = Object) ->
  case module_behavior(Name) of
    elixir_callbacks -> module_behavior(elixir_callbacks:callback_name(Object));
    _ -> []
  end.

module_behavior(Name) ->
  case proplists:get_value(behavior, Name:module_info(attributes)) of
    undefined -> [];
    Else -> hd(Else)
  end.

build_module_form(Line, #elixir_object__{name=Name} = Object, Behavior, Callbacks) ->
  TransFuns   = fun(X) -> build_function_form(Line, Name, X) end,
  TransExport = fun({Fun, Arity}) -> {Fun, Arity - 1} end,
  Functions   = lists:map(TransFuns, Callbacks),
  Export      = lists:map(TransExport, Callbacks),

  [{attribute, Line, module, callback_name(Object)},
   {attribute, Line, behavior, Behavior}, {attribute, Line, export, Export} | Functions].

build_function_form(Line, Module, {Name, ElixirArity}) ->
  Arity = ElixirArity - 1,
  Args  = build_args(Line, Arity, []),
  Const = ?ELIXIR_WRAP_CALL(Line, elixir_constants, lookup, [{atom, Line, Module}]),
  Call  = ?ELIXIR_WRAP_CALL(Line, elixir_dispatch, dispatch, [
    {atom, Line, true}, Const, {atom, Line, Name}, build_list(Line, Args)
  ]),
  {function, Line, Name, Arity,
    [{clause, Line, Args, [], [Call]}]
  }.

build_args(_Line, 0, Acc)    -> Acc;
build_args(Line, Arity, Acc) ->
  Name = ?ELIXIR_ATOM_CONCAT(["X", Arity]),
  Var  = { var, Line, Name },
  build_args(Line, Arity - 1, [Var|Acc]).

build_list(Line, Args)       -> build_list(Line, lists:reverse(Args), {nil, Line}).
build_list(_Line, [], Acc)   -> Acc;
build_list(Line, [H|T], Acc) -> build_list(Line, T, { cons, Line, H, Acc }).