-module(elixir_macro).
-export([dispatch_one/6, format_error/1]).
-include("elixir.hrl").

dispatch_one(Line, Receiver, Name, Args, S, Callback) ->
  case invoke_macros(S#elixir_scope.module) of
    false -> Callback();
    true  ->
      Arity  = length(Args),

      Macros = try
        Receiver:'__macros__'()
      catch
        error:undef -> []
      end,

      case lists:member({Name, Arity}, Macros) of
        true  ->
          ensure_required(Line, Receiver, Name, Arity, S),
          Tree = apply(Receiver, Name, Args),
          NewS = S#elixir_scope{macro={Receiver,Name,Arity}},
          { TTree, TS } = elixir_translator:translate_each(Tree, NewS),
          { TTree, TS#elixir_scope{macro=[]} };
        false -> Callback()
      end
  end.

invoke_macros({_,'::Elixir::Macros'}) -> false;
invoke_macros(_)                      -> true.

%% ERROR HANDLING

ensure_required(Line, Receiver, Name, Arity, S) ->
  Required = [V || { _, V } <- S#elixir_scope.refer],
  case lists:member(Receiver, Required) of
    true  -> ok;
    false ->
      Tuple = { unrequired_macro, { Receiver, Name, Arity, Required } },
      elixir_errors:form_error(Line, S#elixir_scope.filename, ?MODULE, Tuple)
  end.

format_error({unrequired_macro,{Receiver, Name, Arity, Required}}) ->
  io_lib:format("tried to use ~s#~s/~B but module was not required. Required: ~p", [Receiver, Name, Arity, Required]).