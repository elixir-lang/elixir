-module(elixir_remsh).
-export([expand/1]).

expand(Node) ->
    fun (Expr) ->
        case rpc:call(Node,
                      'Elixir-IEx-Autocomplete', expand, [Expr]) of
            {badrpc, _} -> {no, "", []};
            Res -> Res
        end
    end.
