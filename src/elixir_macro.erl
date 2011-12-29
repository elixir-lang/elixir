-module(elixir_macro).
-export([dispatch_one/5]).
-include("elixir.hrl").

dispatch_one(Receiver, Name, Args, S, Callback) ->
  case has_macros(S#elixir_scope.module) of
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
          Tree = apply(Receiver, Name, Args),
          NewS = S#elixir_scope{macro={Receiver,Name,Arity}},
          { TTree, TS } = elixir_translator:translate_each(Tree, NewS),
          { TTree, TS#elixir_scope{macro=[]} };
        false -> Callback()
      end
  end.

has_macros('::Elixir::Macros') -> false;
has_macros(_)                  -> true.