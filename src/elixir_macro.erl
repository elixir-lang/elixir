-module(elixir_macro).
-export([dispatch_one/5]).
-include("elixir.hrl").

dispatch_one(Receiver, Name, Args, S, Callback) ->
  case is_bootstrap(S#elixir_scope.module) of
    true  -> Callback();
    false ->
      Arity = length(Args),
      try
        case lists:member({Name, Arity}, Receiver:'__macros__'()) of
          true  -> 
            Tree = apply(Receiver, Name, Args),
            NewS = S#elixir_scope{macro={Name,Arity}},
            { TTree, TS } = elixir_translator:translate_each(Tree, NewS),
            { TTree, TS#elixir_scope{macro=[]} };
          false -> Callback()
        end
      catch
        error:undef -> Callback()
      end
  end.

is_bootstrap('::Elixir::Macros') -> true;
is_bootstrap(_) -> false.