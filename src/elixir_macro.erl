-module(elixir_macro).
-export([dispatch_one/5]).
-include("elixir.hrl").

dispatch_one(Receiver, Name, Args, S, Callback) ->
  case has_macros(S#elixir_scope.module) of
    false -> Callback();
    true  ->
      Arity = length(Args),
      try
        case lists:member({Name, Arity}, Receiver:'__macros__'()) of
          true  -> 
            Tree = apply(Receiver, Name, unquote_specials(Args, S)),
            NewS = S#elixir_scope{macro={Name,Arity}},
            { TTree, TS } = elixir_translator:translate_each(Tree, NewS),
            { TTree, TS#elixir_scope{macro=[]} };
          false -> Callback()
        end
      catch
        error:undef -> Callback()
      end
  end.

has_macros('::Elixir::Macros') -> false;
has_macros(_)                  -> true.

unquote_specials(Args, S) when is_list(Args) ->
  [unquote_specials(Arg, S) || Arg <- Args];

unquote_specials({ '__MODULE__', _, false }, S) ->
  case S#elixir_scope.module of
    []   -> nil;
    Else -> Else
  end;

unquote_specials({ Left, Line, Right }, S) ->
  { unquote_specials(Left, S), Line, unquote_specials(Right, S) };

unquote_specials({ Left, Right }, S) ->
  { unquote_specials(Left, S), unquote_specials(Right, S) };

unquote_specials(Args, _S) -> Args.