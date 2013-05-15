-module(elixir_literal).
-export([translate/2]).
-import(elixir_translator, [translate_each/2, translate_args/2]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").
-compile({parse_transform, elixir_transform}).

translate({ '<<>>', Meta, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S, allow_tail) of
    error ->
      case S#elixir_scope.context of
        match ->
          build_bitstr(fun elixir_translator:translate_each/2, Args, Meta, nil, S);
        _ ->
          { TArgs, { SC, SV } } = build_bitstr(fun elixir_translator:translate_arg/2, Args, Meta, nil, { S, S }),
          { TArgs, umergec(SV, SC) }
      end;
    Else -> Else
  end;

translate({ '{}', Meta, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S) of
    error ->
      { TArgs, SE } = translate_args(Args, S),
      { { tuple, ?line(Meta), TArgs }, SE };
    Else -> Else
  end;

translate({ '[]', _Meta, [] }, S) ->
  { { nil, 0 }, S };

translate({ '[]', Meta, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S, allow_tail) of
    error ->
      [RTail|RArgs] = lists:reverse(Args),

      case RTail of
        {'|',_,[Left,Right]} ->
          RExprs = [Left|RArgs],
          TailFun = fun(ST) -> translate_each(Right, ST) end;
        _ ->
          RExprs = [RTail|RArgs],
          TailFun = fun(ST) -> { { nil, ?line(Meta) }, ST } end
      end,

      { Exprs, SE } = translate_args(lists:reverse(RExprs), S),
      { Tail, ST }  = TailFun(SE),
      { elixir_tree_helpers:list_to_cons(?line(Meta), Exprs, Tail), ST };
    Else -> Else
  end;

translate({ Left, Right }, S) ->
  translate({ '{}', [], [Left, Right]}, S);

translate(Args, S) when is_list(Args) ->
  translate({ '[]', [], Args }, S);

translate(Number, S) when is_integer(Number) ->
  { { integer, 0, Number }, S };

translate(Number, S) when is_float(Number) ->
  { { float, 0, Number }, S };

translate(Atom, S) when is_atom(Atom) ->
  { { atom, 0, Atom }, S };

translate(Bitstring, S) when is_bitstring(Bitstring) ->
  { elixir_tree_helpers:elixir_to_erl(Bitstring), S }.

%% Helpers

% Build a bitstring taking into accounts the following types:
%
% * If a bitstring or a list is given, we just append its items
% * If '::' is given, extract the bitstring information
% * All the other types are simply translated and handled with Erlang's default
%
build_bitstr(Fun, Exprs, Meta, Env, S) ->
  { Final, FinalS } = build_bitstr_each(Fun, Exprs, Meta, Env, S, []),
  { { bin, ?line(Meta), lists:reverse(Final) }, FinalS }.

build_bitstr_each(_Fun, [], _Meta, _Env, S, Acc) ->
  { Acc, S };

build_bitstr_each(Fun, [{'::',_,[H,V]}|T], Meta, Env, S, Acc) ->
  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  case S of
    { ES, _ } -> [];                      %% translate_arg,  no assigns
    _ -> ES = S#elixir_scope{context=nil} %% translate_each, revert assigns
  end,

  { ExpandedH, ExpandedEnv } = expand(Meta, H, Env, S),
  { Size, Types } = extract_bit_values(Meta, V, ExpandedEnv, ES),
  build_bitstr_each(Fun, T, Meta, ExpandedEnv, S, Acc, ExpandedH, Size, Types);

build_bitstr_each(Fun, [H|T], Meta, Env, S, Acc) ->
  { ExpandedH, ExpandedEnv } = expand(Meta, H, Env, S),
  build_bitstr_each(Fun, T, Meta, ExpandedEnv, S, Acc, ExpandedH, default, default).

expand(_Meta, H, Env, _S) when is_integer(H); is_float(H); is_atom(H); is_atom(H); is_bitstring(H) ->
  { H, Env };

expand(Meta, H, nil, { S, _ }) ->
  expand(Meta, H, elixir_scope:to_ex_env({ ?line(Meta), S }), S);

expand(Meta, H, nil, S) ->
  expand(Meta, H, elixir_scope:to_ex_env({ ?line(Meta), S }), S);

expand(_Meta, H, Env, _S) ->
  { 'Elixir.Macro':expand(H, Env), Env }.

build_bitstr_each(Fun, T, Meta, Env, S, Acc, H, Size, Types) when is_list(H) ->
  case is_default_or_utf(Types) of
    true ->
      { NewAcc, NewS } = lists:foldl(fun(L, { LA, LS }) ->
        { FL, FS } = Fun(L, LS),
        { [{ bin_element, ?line(Meta), FL, Size, Types }|LA], FS }
      end, { Acc, S }, H),
      build_bitstr_each(Fun, T, Meta, Env, NewS, NewAcc);
    false ->
      build_bitstr_default(Fun, T, Meta, Env, S, Acc, H, Size, Types)
  end;

build_bitstr_each(Fun, T, Meta, Env, S, Acc, H, Size, Types) when is_bitstring(H) ->
  case is_default_or_utf(Types) of
    true ->
      Line = ?line(Meta),
      { bin, _, Elements } = elixir_tree_helpers:elixir_to_erl(H),
      NewAcc = lists:foldl(fun({ bin_element, _, Expr, _, _ }, FinalAcc) ->
        [{ bin_element, Line, Expr, Size, Types }|FinalAcc]
      end, Acc, Elements),
      build_bitstr_each(Fun, T, Meta, Env, S, NewAcc);
    false ->
      build_bitstr_default(Fun, T, Meta, Env, S, Acc, H, Size, Types)
  end;

build_bitstr_each(Fun, T, Meta, Env, S, Acc, H, Size, Types) ->
  build_bitstr_default(Fun, T, Meta, Env, S, Acc, H, Size, Types).

build_bitstr_default(Fun, T, Meta, Env, S, Acc, H, Size, Types) ->
  { Expr, NS } = Fun(H, S),
  build_bitstr_each(Fun, T, Meta, Env, NS, [{ bin_element, ?line(Meta), Expr, Size, Types }|Acc]).

is_default_or_utf(default) -> true;
is_default_or_utf([UTF|_]) when UTF == utf8; UTF == utf16; UTF == utf32 -> true;
is_default_or_utf([_|T]) -> is_default_or_utf(T);
is_default_or_utf([]) -> false.

%% Extra bitstring specifiers

extract_bit_values(Meta, V, Env, S) when is_list(V) ->
  extract_bit_values(Meta, V, default, [], Env, S);

extract_bit_values(Meta, V, Env, S) ->
  extract_bit_values(Meta, [V], Env, S).

extract_bit_values(Meta, [{ Value, _Meta, Atom }|T] = All, Size, Types, Env, S)
    when is_atom(Value) andalso (is_atom(Atom) orelse Atom == []) ->
  case extract_bit_type(Value, Types) of
    { error, unknown } ->
      handle_unknown_specifier(Meta, All, Size, Types, Env, S);
    NewTypes when is_list(NewTypes) ->
      extract_bit_values(Meta, T, Size, NewTypes, Env, S)
  end;

extract_bit_values(Meta, [{ Value, CallMeta, [_] = Args }|T] = All, Size, Types, Env, S) when is_atom(Value) ->
  { TArgs, _ } = elixir_translator:translate_args(Args, S),
  case extract_bit_type_or_size(Value, TArgs, Size, Types) of
    { error, unknown } ->
      handle_unknown_specifier(Meta, All, Size, Types, Env, S);
    { error, Msg } ->
      elixir_errors:syntax_error(CallMeta, S#elixir_scope.file, Msg);
    { NewSize, NewTypes } ->
      extract_bit_values(Meta, T, NewSize, NewTypes, Env, S)
  end;

extract_bit_values(Meta, [Size|T], default, Types, Env, S) when is_integer(Size) andalso Size >= 0 ->
  extract_bit_values(Meta, T, {integer, Meta, Size}, Types, Env, S);

extract_bit_values(Meta, [{ '|', _, [Left, Right] }], Size, Types, Env, S) ->
  { Expanded, ExpandedEnv } = expand(Meta, Right, Env, S),
  extract_bit_values(Meta, [Left|join_expansion(Expanded,[])], Size, Types, ExpandedEnv, S);

extract_bit_values(Meta, [_|_] = All, Size, Types, Env, S) ->
  handle_unknown_specifier(Meta, All, Size, Types, Env, S);

extract_bit_values(_Meta, [], Size, [], _Env, _S) ->
  { Size, default };

extract_bit_values(_Meta, [], Size, Types, _Env, _S) ->
  { Size, lists:reverse(Types) }.

extract_bit_type(Value, Types) when
    Value == binary; Value == integer; Value == float; Value == bitstring;
    Value == bytes; Value == bits;
    Value == utf8; Value == utf16; Value == utf32;
    Value == signed; Value == unsigned;
    Value == big; Value == little; Value == native ->
  [Value|Types];

extract_bit_type(_Value, _Types) ->
  { error, unknown }.

extract_bit_type_or_size(size, [{ Kind, _, _ } = Arg], default, Types) when Kind == var; Kind == integer ->
  { Arg, Types };

extract_bit_type_or_size(size, _Args, default, _Types) ->
  { error, "size in bitstring expects an integer or a variable as argument" };

extract_bit_type_or_size(size, _Args, _Other, _Types) ->
  { error, "duplicated size definition for bitstring" };

extract_bit_type_or_size(unit, [{ integer, _, Arg }], Size, Types) ->
  { Size, [{ unit, Arg }|Types] };

extract_bit_type_or_size(unit, _Args, _Other, _Types) ->
  { error, "unit in bitstring expects an integer as argument" };

extract_bit_type_or_size(_Value, _Args, _Other, _Types) ->
  { error, unknown }.

handle_unknown_specifier(Meta, [H|T], Size, Types, Env, S) ->
  case expand(Meta, H, Env, S) of
    { H, _ } ->
      elixir_errors:syntax_error(Meta, S#elixir_scope.file, "unknown bitstring specifier ~ts", ['Elixir.Macro':to_binary(H)]);
    { E, ExpandedEnv } ->
      extract_bit_values(Meta, join_expansion(E,T), Size, Types, ExpandedEnv, S)
  end.

join_expansion({ '__block__', _, [Expanded] }, Tail) -> join_expansion(Expanded, Tail);
join_expansion(Expanded, Tail) when is_list(Expanded) -> Expanded ++ Tail;
join_expansion(Expanded, Tail) -> [Expanded|Tail].
