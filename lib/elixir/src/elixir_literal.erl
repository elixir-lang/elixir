%% Handle translation of Elixir literals to Erlang AST.
-module(elixir_literal).
-export([translate/2]).
-import(elixir_translator, [translate_each/2, translate_args/2]).
-import(elixir_scope, [umergec/2]).
-include("elixir.hrl").

translate({ '<<>>', Meta, Args } = Original, S) when is_list(Args) ->
  case elixir_partials:handle(Original, S, allow_tail) of
    error ->
      case S#elixir_scope.context of
        match ->
          build_bitstr(fun elixir_translator:translate_each/2, Args, Meta, S);
        _ ->
          { TArgs, { SC, SV } } = build_bitstr(fun elixir_translator:translate_arg/2, Args, Meta, { S, S }),
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
      { elixir_utils:list_to_cons(?line(Meta), Exprs, Tail), ST };
    Else -> Else
  end;

translate({ Left, Right }, S) ->
  translate({ '{}', [], [Left, Right]}, S);

translate(Args, S) when is_list(Args) ->
  translate({ '[]', [], Args }, S);

translate(Tuple, S) when is_tuple(Tuple) ->
  elixir_errors:compile_error(0, S#elixir_scope.file,
    "tuples in quoted expressions must have 2 or 3 items, invalid quoted expression: ~ts",
    ['Elixir.Kernel':inspect(Tuple)]);

translate(Other, S) ->
  { elixir_utils:elixir_to_erl(0, Other, S), S }.

%% Helpers

% Build a bitstring taking into accounts the following types:
%
% * If a bitstring or a list is given, we just append its items
% * If '::' is given, extract the bitstring information
% * All the other types are simply translated and handled with Erlang's default
%
build_bitstr(Fun, Exprs, Meta, S) ->
  { Final, FinalS } = build_bitstr_each(Fun, Exprs, Meta, S, []),
  { { bin, ?line(Meta), lists:reverse(Final) }, FinalS }.

build_bitstr_each(_Fun, [], _Meta, S, Acc) ->
  { Acc, S };

build_bitstr_each(Fun, [{'::',_,[H,V]}|T], Meta, S, Acc) ->
  %% Variables defined outside the binary can be accounted
  %% on subparts, however we can't assign new variables.
  case S of
    { ES, _ } -> [];                      %% translate_arg,  no assigns
    _ -> ES = S#elixir_scope{context=nil} %% translate_each, revert assigns
  end,

  { Size, Types } = extract_bit_values(Meta, V, ES),
  build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types);

build_bitstr_each(Fun, [H|T], Meta, S, Acc) ->
  build_bitstr_each(Fun, T, Meta, S, Acc, H, default, default).

build_bitstr_each(Fun, T, Meta, S, Acc, H, Size, Types) ->
  { Expr, NS } = Fun(H, S),

  AllowString = types_allow_string(Types),
  AllowSplice = types_allow_splice(Types),
  AllowAny    = (AllowString orelse AllowSplice) andalso (Size == default),

  case AllowAny andalso Expr of
    { bin, _, [{ bin_element, 0, { string, 0, String }, default, default }] } when AllowString ->
      build_bitstr_each(Fun, T, Meta, NS, [{ bin_element, ?line(Meta), { string, 0, String }, Size, Types }|Acc]);
    { bin, _, Elements } when AllowSplice ->
      build_bitstr_each(Fun, T, Meta, NS, lists:reverse(Elements) ++ Acc);
    { cons, _, _, _ } = Cons ->
      build_bitstr_each(Fun, T, Meta, NS, rehash_cons(Cons, Size, Types, []) ++ Acc);
    { nil, _ } ->
      build_bitstr_each(Fun, T, Meta, NS, Acc);
    _ ->
      build_bitstr_each(Fun, T, Meta, NS, [{ bin_element, ?line(Meta), Expr, Size, Types }|Acc])
  end.

rehash_cons({ nil, _ }, _Size, _Types, Acc) -> Acc;
rehash_cons({ cons, Line, Left, Right }, Size, Types, Acc) ->
  rehash_cons(Right, Size, Types, [{ bin_element, Line, Left, Size, Types }|Acc]).

types_allow_string([End|T]) when End == little; End == big -> types_allow_string(T);
types_allow_string([UTF|T]) when UTF == utf8; UTF == utf16; UTF == utf32 -> types_allow_string(T);
types_allow_string([]) -> true;
types_allow_string(_) -> false.

types_allow_splice(default) -> true;
types_allow_splice([bytes]) -> true;
types_allow_splice([binary]) -> true;
types_allow_splice([bits]) -> true;
types_allow_splice([bitstring]) -> true;
types_allow_splice(_) -> false.

%% Extra bitstring specifiers

extract_bit_values(Meta, V, S) when is_list(V) ->
  extract_bit_values(Meta, V, default, [], S);

extract_bit_values(Meta, V, S) ->
  extract_bit_values(Meta, [V], S).

extract_bit_values(Meta, [{ Value, _Meta, Atom }|T] = All, Size, Types, S)
    when is_atom(Value) andalso (is_atom(Atom) orelse Atom == []) ->
  case extract_bit_type(Value, Types) of
    { error, unknown } ->
      handle_unknown_specifier(Meta, All, Size, Types, S);
    NewTypes when is_list(NewTypes) ->
      extract_bit_values(Meta, T, Size, NewTypes, S)
  end;

extract_bit_values(Meta, [{ Value, CallMeta, [_] = Args }|T] = All, Size, Types, S) when is_atom(Value) ->
  { TArgs, _ } = elixir_translator:translate_args(Args, S),
  case extract_bit_type_or_size(Value, TArgs, Size, Types) of
    { error, unknown } ->
      handle_unknown_specifier(Meta, All, Size, Types, S);
    { error, Msg } ->
      elixir_errors:syntax_error(CallMeta, S#elixir_scope.file, Msg);
    { NewSize, NewTypes } ->
      extract_bit_values(Meta, T, NewSize, NewTypes, S)
  end;

extract_bit_values(Meta, [Size|T], default, Types, S) when is_integer(Size) andalso Size >= 0 ->
  extract_bit_values(Meta, T, {integer, Meta, Size}, Types, S);

extract_bit_values(Meta, [{ '|', _, [Left, Right] }], Size, Types, S) ->
  { Expanded, ES } = 'Elixir.Macro':expand_all(Right, elixir_scope:to_ex_env({ ?line(Meta), S }), S),
  extract_bit_values(Meta, [Left|join_expansion(Expanded,[])], Size, Types, ES);

extract_bit_values(Meta, [_|_] = All, Size, Types, S) ->
  handle_unknown_specifier(Meta, All, Size, Types, S);

extract_bit_values(_Meta, [], Size, [], _S) ->
  { Size, default };

extract_bit_values(_Meta, [], Size, Types, _S) ->
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

handle_unknown_specifier(Meta, [H|T], Size, Types, S) ->
  case 'Elixir.Macro':expand_all(H, elixir_scope:to_ex_env({ ?line(Meta), S }), S) of
    { H, ES } ->
      elixir_errors:syntax_error(Meta, ES#elixir_scope.file, "unknown bitstring specifier ~ts", ['Elixir.Macro':to_string(H)]);
    { E, ES } ->
      extract_bit_values(Meta, join_expansion(E,T), Size, Types, ES)
  end.

join_expansion({ '__block__', _, [Expanded] }, Tail) -> join_expansion(Expanded, Tail);
join_expansion(Expanded, Tail) when is_list(Expanded) -> Expanded ++ Tail;
join_expansion(Expanded, Tail) -> [Expanded|Tail].
