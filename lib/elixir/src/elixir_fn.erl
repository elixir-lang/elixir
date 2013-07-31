-module(elixir_fn).
-export([fn/3, capture/3]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").

capture(Meta, { '/', _, [{ { '.', _, [M, F] }, _ , [] }, A] }, S) when is_atom(F), is_integer(A) ->
  { [MF, FF, AF], SF } = elixir_translator:translate_args([M, F, A], S),
  { { 'fun', ?line(Meta), { function, MF, FF, AF } }, SF };

capture(Meta, { '/', _, [{ F, _, C }, A] }, S) when is_atom(F), is_integer(A), is_atom(C) ->
  WrappedMeta =
    case lists:keyfind(import_fa, 1, Meta) of
      { import_fa, { Receiver, Context } } ->
        lists:keystore(context, 1,
          lists:keystore(import, 1, Meta, { import, Receiver }),
          { context, Context }
        );
      false -> Meta
    end,

  case elixir_dispatch:import_function(WrappedMeta, F, A, S) of
    false -> elixir_errors:compile_error(WrappedMeta, S#elixir_scope.file,
                                         "expected ~ts/~B to be a function, but it is a macro", [F, A]);
    Else  -> Else
  end.

fn(Meta, Clauses, S) ->
  Transformer = fun({ ArgsWithGuards, CMeta, Expr }, Acc) ->
    { Args, Guards } = elixir_clauses:extract_splat_guards(ArgsWithGuards),
    elixir_clauses:assigns_block(?line(CMeta), fun elixir_translator:translate/2, Args, [Expr], Guards, umergec(S, Acc))
  end,

  { TClauses, NS } = lists:mapfoldl(Transformer, S, Clauses),
  Arities = [length(Args) || { clause, _Line, Args, _Guards, _Exprs } <- TClauses],

  case length(lists:usort(Arities)) of
    1 ->
      { { 'fun', ?line(Meta), { clauses, TClauses } }, umergec(S, NS) };
    _ ->
      elixir_errors:syntax_error(Meta, S#elixir_scope.file,
                                 "cannot mix clauses with different arities in function definition")
  end.
