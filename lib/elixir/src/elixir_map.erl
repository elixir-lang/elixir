-module(elixir_map).
-export([translate/3, expand/3]).
-include("elixir.hrl").

expand(Meta, [{ '|', UpdateMeta, [Left, Right]}], E) ->
  { [ELeft|ERight], EA } = elixir_exp:expand_args([Left|Right], E),
  { { '%{}', Meta, [{ '|', UpdateMeta, [ELeft, ERight] }] }, EA };
expand(Meta, Args, E) ->
  { EArgs, EA } = elixir_exp:expand_args(Args, E),
  { { '%{}', Meta, EArgs }, EA }.

translate(Meta, Args, #elixir_scope{extra=Extra} = RS) ->
  { Assocs, TUpdate, S } = extract_assoc_update(Args, RS),
  { Op, KeyFun, ValFun } = extract_key_val_op(TUpdate, S),

  Line = ?line(Meta),

  { TArgs, SA } = lists:mapfoldl(fun({ Key, Value }, Acc) ->
    { TKey, Acc1 }   = KeyFun(Key, Acc),
    { TValue, Acc2 } = ValFun(Value, Acc1#elixir_scope{extra=Extra}),
    { { Op, ?line(Meta), TKey, TValue }, Acc2 }
  end, S, Assocs),

  build_map(Line, TUpdate, TArgs, SA).

extract_assoc_update([{'|', _Meta, [Update, Args]}], SA) ->
  { TArg, SA1 } = elixir_translator:translate_arg(Update, SA, SA),
  { Args, TArg, SA1 };
extract_assoc_update(Args, SA) -> { Args, nil, SA }.

extract_key_val_op(_TUpdate, #elixir_scope{context=match}) ->
  { map_field_exact,
    fun(X, Acc) -> elixir_translator:translate(X, Acc#elixir_scope{extra=map_key}) end,
    fun elixir_translator:translate/2 };
extract_key_val_op(TUpdate, S) ->
  KS = #elixir_scope{extra=map_key},
  Op = if TUpdate == nil -> map_field_assoc; true -> map_field_exact end,
  { Op,
    fun(X, Acc) -> elixir_translator:translate_arg(X, Acc, KS) end,
    fun(X, Acc) -> elixir_translator:translate_arg(X, Acc, S) end }.

build_map(Line, nil, TArgs, SA) -> { { map, Line, TArgs }, SA };
build_map(Line, TUpdate, TArgs, SA) -> { { map, Line, TUpdate, TArgs }, SA }.
