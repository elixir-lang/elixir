-module(elixir_macro).
-export([dispatch_one/5]).
-include("elixir.hrl").

dispatch_one(Receiver, Name, Args, S, Callback) ->
  case is_bootstrap(S#elixir_scope.namespace) of
    true  -> Callback();
    false ->
      try
        case lists:member({Name, length(Args)}, Receiver:'__macros__'()) of
          true  -> 
            Tree = apply(Receiver, Name, Args),
            elixir_translator:translate_each(Tree, S);
          false -> Callback()
        end
      catch
        error:undef -> Callback()
      end
  end.

is_bootstrap('::Elixir::Namespace') -> true;
is_bootstrap(_) -> false.